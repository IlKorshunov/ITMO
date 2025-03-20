#include "return_codes.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#ifdef ZLIB
#include <zlib.h>
#elif defined(LIBDEFLATE)
#include <libdeflate.h>
#elif defined(ISAL)
#include <isa-l/isa-l.h>
#else
#error "Not found library"
#endif


size_t line = 0;

typedef enum
{
	UNKOWN,
	IHDR,
	PLTE,
	IDAT,
	IEND
} ChunkType;

typedef struct
{
	ChunkType type;
	size_t size;
	unsigned char *inf;
} Chunk;

Chunk create_Chunk(void)
{
	Chunk out;
	out.size = 0;
	out.type = UNKOWN;
	out.inf = NULL;
	return out;
}

void chunk_clear(Chunk *chunk)
{
	if (chunk->inf != NULL)
	{
		free(chunk->inf);
		chunk->inf = NULL;
	}
	chunk->size = 0;
	chunk->type = UNKOWN;
}

typedef enum
{
	NONE,
	P5,
	P6
} ImageType;

typedef struct
{
	unsigned char *PLTE_array;
	unsigned char *outPalet;
	unsigned char *final;
	unsigned char *data;
	unsigned char *inf_all;
	unsigned int image_width;
	unsigned int image_height;
	ImageType type;
	unsigned char pixel_len;
	unsigned int image_size;
	unsigned int type_color;
} Image;

Image create_Image(void)
{
	Image out;
	out.image_size = 0;
	out.type_color = 0;
	out.type = NONE;
	out.image_width = 0;
	out.pixel_len = 0;
	out.inf_all = NULL;
	out.image_height = 0;
	out.PLTE_array = 0;
	out.outPalet = 0;
	out.final = 0;
	out.data = 0;
	return out;
}

Image create_Image(void);
Chunk create_Chunk(void);

void image_clear(Image *img)
{
	if (img->inf_all != NULL)
	{
		free(img->inf_all);
		img->inf_all = NULL;
	}
	img->image_size = 0;
	img->type_color = 0;
	img->type = NONE;
	img->image_width = 0;
	img->pixel_len = 0;
	img->image_height = 0;
}

int checkFileError(FILE *file)
{
	if (feof(file) || ferror(file))
	{
		fprintf(stderr, "Error: unexpected end of file or file read error\n");
		return ERROR_UNKNOWN;
	}
	return SUCCESS;
}

int readData(FILE *file, void *in, size_t size, const char *error_message)
{
	if (fread(in, sizeof(unsigned char), size, file) != size)
	{
		fprintf(stderr, "Error: %s\n", error_message);
		return ERROR_DATA_INVALID;
	}
	return SUCCESS;
}

int checkChunk(int first, int second, Image *image, Chunk *chunk, const char *error_message)
{
	int mistake = SUCCESS;
	if (first != second)
	{
		fprintf(stderr, "Error: %s\n", error_message);
		mistake = ERROR_DATA_INVALID;
		chunk_clear(chunk);
		image_clear(image);
		return mistake;
	}
	return mistake;
}

int get_Chunk(FILE *file, Chunk *chunk)
{
	int mistake = SUCCESS;

	char size_str[4];
	mistake = readData(file, size_str, 4, "unable to read chunk size");
	if (mistake != SUCCESS)
	{
		chunk_clear(chunk);
		return mistake;
	}
	mistake = checkFileError(file);
	if (mistake != SUCCESS)
	{
		chunk_clear(chunk);
		return mistake;
	}

	chunk->size = (unsigned char)size_str[0] << 24 | (unsigned char)size_str[1] << 16 |
				  (unsigned char)size_str[2] << 8 | (unsigned char)size_str[3];

	char type_str[5];
	mistake = readData(file, type_str, 4, "unable to read chunk type");
	if (mistake != SUCCESS)
	{
		chunk_clear(chunk);
		return mistake;
	}
	type_str[4] = '\0';
	switch (type_str[0])
	{
	case 'I':
		if (strncmp(type_str, "IHDR", 4) == 0)
		{
			chunk->type = IHDR;
		}
		else if (strncmp(type_str, "IEND", 4) == 0)
		{
			chunk->type = IEND;
		}
		else if (strncmp(type_str, "IDAT", 4) == 0)
		{
			chunk->type = IDAT;
		}
		break;
	case 'P':
		if (strncmp(type_str, "PLTE", 4) == 0)
		{
			chunk->type = PLTE;
		}
		break;
	default:
		break;
	}

	chunk->inf = (unsigned char *)malloc(chunk->size);

	if (chunk->inf == NULL)
	{
		fprintf(stderr, "Can't allocate memory\n");
		mistake = ERROR_OUT_OF_MEMORY;
		chunk_clear(chunk);
		return mistake;
	}

	mistake = readData(file, chunk->inf, chunk->size, "Error in chunk inf");
	if (mistake != SUCCESS)
	{
		chunk_clear(chunk);
		return mistake;
	}
	mistake = checkFileError(file);
	if (mistake != SUCCESS)
	{
		chunk_clear(chunk);
		return mistake;
	}

	unsigned char buffer[4];
	mistake = readData(file, buffer, 4, "Error in CRC");
	if (mistake != SUCCESS)
	{
		chunk_clear(chunk);
		return mistake;
	}
	mistake = checkFileError(file);
	if (mistake != SUCCESS)
	{
		fprintf(stderr, "Error in CRC\n");
		chunk_clear(chunk);
		return mistake;
	}

	return mistake;
}

int get_IHDR(Image *image, Chunk *chunk)
{
	int mistake = SUCCESS;

	int depth;
	int compression;
	int filtering;
	int interlace;

	image->image_width =
		(unsigned char)chunk->inf[0] << 24 | (unsigned char)chunk->inf[1] << 16 | (unsigned char)chunk->inf[2] << 8 |
		(unsigned char)chunk->inf[3];

	image->image_height =
		(unsigned char)chunk->inf[4] << 24 | (unsigned char)chunk->inf[5] << 16 | (unsigned char)chunk->inf[6] << 8 |
		(unsigned char)chunk->inf[7];

	depth = (int)chunk->inf[8];
	checkChunk(depth, 8, image, chunk, "Incorrect depth");

	image->type_color = (int)chunk->inf[9];
	if (image->type_color != 2 && image->type_color != 0 && image->type_color != 3)
	{
		fprintf(stderr, "Incorrect type_color");
		chunk_clear(chunk);
		image_clear(image);
		mistake = ERROR_UNSUPPORTED;
		return mistake;
	}

	if (image->type_color == 2)
	{
		image->pixel_len = 3;
	}
	else
	{
		image->pixel_len = 1;
	}

	if (image->type_color == 3 || image->type_color == 2)
	{
		image->type = P6;
	}
	else
	{
		image->type = P5;
	}

	compression = (int)chunk->inf[10];
	checkChunk(compression, 0, image, chunk, "Unknown compression");

	filtering = (int)chunk->inf[11];
	checkChunk(filtering, 0, image, chunk, "Unknown filtering");

	interlace = (int)chunk->inf[12];
	if (interlace != 1 && interlace != 0)
	{
		fprintf(stderr, "Incorrect interlace");
		chunk_clear(chunk);
		image_clear(image);
		mistake = ERROR_DATA_INVALID;
		return mistake;
	}

	return mistake;
}

int DATA(Image *image, Chunk *chunk)
{
	unsigned char *buffer = (unsigned char *)malloc(image->image_size + chunk->size);
	if (buffer == NULL)
	{
		fprintf(stderr, "Not enough memory to handle decoding\n");
		return ERROR_OUT_OF_MEMORY;
	}
	if (image->image_size != 0)
	{
		memcpy(buffer, image->inf_all, image->image_size);
	}
	memcpy(buffer + image->image_size, chunk->inf, chunk->size);
	image->image_size += chunk->size;
	free(image->inf_all);
	image->inf_all = buffer;
	return SUCCESS;
}

int get_PLTE(Image *image, Chunk *chunk)
{
	image->PLTE_array = (unsigned char *)malloc(chunk->size);
	if (chunk->size == 0)
	{
		fprintf(stderr, "size PLTE = 0\n");
		chunk_clear(chunk);
		image_clear(image);
		return ERROR_DATA_INVALID;
	}
	if (image->type_color == 0)
	{
		fprintf(stderr, "Incorrect type PLTE in colortype = 1\n");
		chunk_clear(chunk);
		image_clear(image);
		return ERROR_DATA_INVALID;
	}
	if (image->PLTE_array == NULL)
	{
		fprintf(stderr, "Not enough memory to handle decoding\n");
		chunk_clear(chunk);
		image_clear(image);
		return ERROR_OUT_OF_MEMORY;
	}
	memcpy(image->PLTE_array, chunk->inf, chunk->size);
	int flag = 1;
	for (int i = 0; i < chunk->size; i = i + 3)
	{
		if (image->PLTE_array[i] == image->PLTE_array[i + 1] == image->PLTE_array[i + 2])
		{
			flag = 0;
		}
		else
		{
			flag = 1;
			break;
		}
	}
	if (flag == 0)
	{
		image->type = P5;
	}
	return SUCCESS;
}

int inflateMy(size_t inSize, unsigned char *inData, size_t outSize, unsigned char *outData)
{
#if defined(ZLIB)
	uLongf destLen = outSize;
	int res = uncompress(outData, &destLen, inData, inSize);

	if (res == Z_OK)
	{
		return SUCCESS;
	}
	else if (res == Z_MEM_ERROR)
	{
		return ERROR_OUT_OF_MEMORY;
	}
	else if (res == Z_BUF_ERROR)
	{
		return ERROR_UNKNOWN;
	}
	else
	{
		return ERROR_DATA_INVALID;
	}
}
#elif defined(LIBDEFLATE)
	int mistake = SUCCESS;

	struct libdeflate_decompressor *decompressor = libdeflate_alloc_decompressor();

	if (!decompressor)
	{
		fprintf(stderr, "Failed to allocate decompressor\n");
		libdeflate_free_decompressor(decompressor);
		return 1;
	}

	int actual_out_size = libdeflate_zlib_decompress(decompressor, inData, inSize, outData, outSize, NULL);
	if (actual_out_size != 0)
	{
		fprintf(stderr, "Failed to decompress data\n");
		libdeflate_free_decompressor(decompressor);
		return 1;
	}
	libdeflate_free_decompressor(decompressor);
	return mistake;
}
#elif defined(ISAL)
	int mistake = SUCCESS;

	struct inflate_state state;
	isal_inflate_init(&state);

	state.next_in = inData;
	state.avail_in = inSize;
	state.next_out = outData;
	state.avail_out = outSize;

	struct isal_zlib_header header;
	isal_read_zlib_header(&state, &header);
	int ret = isal_inflate(&state);

	if (ret != ISAL_DECOMP_OK)
	{
		printf("Failed to decompress data: %d\n", ret);
		mistake = ERROR_DATA_INVALID;
	}

	return mistake;
}
#else
#error Not supported;
#endif

int Decompression(Image *image)
{
	int mistake = SUCCESS;
	line = (image->pixel_len * image->image_width + 1);
	image->final = (unsigned char *)malloc(line * image->image_height);
	if (NULL == image->final)
	{
		mistake = ERROR_OUT_OF_MEMORY;
		image_clear(image);
		return mistake;
	}
	mistake = inflateMy(image->image_size, image->inf_all, line * image->image_height, image->final);

	if (mistake == SUCCESS)
	{
		image->data = (unsigned char *)malloc(image->pixel_len * image->image_height * image->image_width);
		if (NULL == image->data)
		{
			image_clear(image);
			mistake = ERROR_OUT_OF_MEMORY;
			return mistake;
		}
		if (image->type_color == 3)
		{
			image->outPalet = (unsigned char *)malloc(3 * image->pixel_len * image->image_height * image->image_width);
			if (NULL == image->outPalet)
			{
				image_clear(image);
				mistake = ERROR_OUT_OF_MEMORY;
				return mistake;
			}
		}
		else
		{
			image->outPalet = (unsigned char *)malloc(image->pixel_len * image->image_height * image->image_width);
			if (NULL == image->outPalet)
			{
				image_clear(image);
				mistake = ERROR_OUT_OF_MEMORY;
				return mistake;
			}
		}
	}
	return mistake;
}

void decoding(Image *image)
{
	int now = 0;
	int now_out = 0;
	int shift = 0;
	int lef_val, up_val, lefup_val;
	int lef_inter, up_inter, lefup_inter;

	for (size_t i = 0; i < image->image_height; i++)
	{
		int way = (int)image->final[i * line];
		for (size_t j = 1; j < line; j++)
		{
			size_t left = i * image->image_width * image->pixel_len + j - image->pixel_len - 1;
			size_t up = (i - 1) * image->image_width * image->pixel_len + j - 1;
			size_t lefup = (i - 1) * image->image_width * image->pixel_len + j - image->pixel_len - 1;
			if (way == 0)
			{
				shift = 0;
			}
			else if (way == 1)
			{
				if (j <= image->pixel_len)
				{
					shift = 0;
				}
				else
				{
					shift = image->data[left];
				}
			}
			else if (way == 2)
			{
				if (i == 0)
				{
					shift = 0;
				}
				else
				{
					shift = image->data[up];
				}
			}
			else if (way == 3)
			{
				shift = 0;
				if (i != 0)
				{
					shift += image->data[up];
				}
				if (j > image->pixel_len)
				{
					shift += image->data[left];
				}
				shift = shift / 2;
			}
			else if (way == 4)
			{
				if (i == 0)
				{
					up_val = 0;
				}
				else
				{
					up_val = image->data[up];
				}
				if (j <= image->pixel_len)
				{
					lef_val = 0;
				}
				else
				{
					lef_val = image->data[left];
				}
				if (i == 0 || j <= image->pixel_len)
				{
					lefup_val = 0;
				}
				else
				{
					lefup_val = image->data[lefup];
				}
				shift = lef_val + up_val - lefup_val;

				up_inter = abs(shift - up_val);
				lef_inter = abs(shift - lef_val);
				lefup_inter = abs(shift - lefup_val);

				int first;
				int second;

				if (lef_inter < lefup_inter)
				{
					first = lef_inter;
				}
				else
				{
					first = lefup_inter;
				}

				if (up_inter < first)
				{
					second = up_inter;
				}
				else
				{
					second = first;
				}

				if (second == up_inter)
				{
					shift = up_val;
				}
				else if (second == lef_inter)
				{
					shift = lef_val;
				}
				else
				{
					shift = lefup_val;
				}
			}
			image->data[now++] = (unsigned char)((shift + (int)image->final[i * line + j]) & 255);
			if (image->type_color != 3)
			{
				image->outPalet[now_out++] = (unsigned char)((shift + (int)image->final[i * line + j]) & 255);
			}
		}
	}
	if (image->type_color == 3)
	{
		for (int i = 0; i < image->image_height; ++i)
		{
			for (int j = 0; j < image->image_width; ++j)
			{
				if (image->type == P6)
				{
					image->outPalet[i * image->image_width * 3 + 3 * j] =
						image->PLTE_array[3 * image->data[i * image->image_width + j]];
					image->outPalet[i * image->image_width * 3 + 3 * j + 1] =
						image->PLTE_array[3 * image->data[i * image->image_width + j] + 1];
					image->outPalet[i * image->image_width * 3 + 3 * j + 2] =
						image->PLTE_array[3 * image->data[i * image->image_width + j] + 2];
				}
				else
				{
					image->outPalet[i * image->image_width * 3 + 3 * j] =
						image->PLTE_array[3 * image->data[i * image->image_width + j]];
				}
			}
		}
	}
	if (image->final != NULL)
	{
		free(image->final);
	}
	if (image->PLTE_array != NULL)
	{
		free(image->PLTE_array);
	}
	free(image->data);
}

int get_Image(char *filename, Image *image)
{
	int mistake = SUCCESS;
	char png_signature[] = "\x89PNG\r\n\x1A\n";
	char in_signature[8];
	FILE *file = fopen(filename, "rb");
	if (NULL == file)
	{
		fprintf(stderr, "Error during opening file");
		mistake = ERROR_CANNOT_OPEN_FILE;
		image_clear(image);
		return ERROR_CANNOT_OPEN_FILE;
	}
	mistake = readData(file, in_signature, 8, "wrong first 8 bait");
	if (mistake != SUCCESS)
	{
		image_clear(image);
		fclose(file);
		return ERROR_DATA_INVALID;
	}
	if (memcmp(in_signature, png_signature, 8) != 0)
	{
		fprintf(stderr, "It isn't PNG format\n");
		image_clear(image);
		fclose(file);
		return ERROR_DATA_INVALID;
	}
	int flag = 1;
	int count_1 = 0;
	int count_2 = 0;
	int end = 0;
	while (flag)
	{
		Chunk chunk = create_Chunk();
		mistake = get_Chunk(file, &chunk);
		if (mistake != SUCCESS)
		{
			fclose(file);
			image_clear(image);
			chunk_clear(&chunk);
			return mistake;
		}
		switch (chunk.type)
		{
		case IHDR:
			mistake = get_IHDR(image, &chunk);
			if (mistake != SUCCESS)
			{
				fclose(file);
				chunk_clear(&chunk);
				image_clear(image);
				return ERROR_DATA_INVALID;
			}
			count_1++;
			if (count_1 > 1)
			{
				fclose(file);
				chunk_clear(&chunk);
				image_clear(image);
				fprintf(stderr, "You have more than one IHDR chunk\n");
				return ERROR_UNKNOWN;
			}
			break;
		case PLTE:
			mistake = get_PLTE(image, &chunk);
			count_2++;
			if (chunk.size == 0 || chunk.size % 3 != 0 || chunk.size > 256 * 3)
			{
				fprintf(stderr, "Incorrect PLTE\n");
				fclose(file);
				chunk_clear(&chunk);
				image_clear(image);
				return ERROR_DATA_INVALID;
			}
			if (count_2 > 1)
			{
				mistake = ERROR_UNKNOWN;
				fclose(file);
				chunk_clear(&chunk);
				image_clear(image);
				fprintf(stderr, "You have more than one PLTE chunk\n");
				return mistake;
			}
			if (mistake != SUCCESS)
			{
				fclose(file);
				chunk_clear(&chunk);
				image_clear(image);
				return mistake;
			}
			break;
		case IDAT:
			mistake = DATA(image, &chunk);
			if (mistake != SUCCESS)
			{
				fclose(file);
				chunk_clear(&chunk);
				image_clear(image);
				return mistake;
			}
			break;
		case IEND:
			end++;
			if (chunk.size != 0)
			{
				fprintf(stderr, "Incorrect IEND\n");
				fclose(file);
				chunk_clear(&chunk);
				image_clear(image);
				return ERROR_DATA_INVALID;
			}
			flag = 0;
			mistake = Decompression(image);
			if (mistake != SUCCESS)
			{
				fclose(file);
				chunk_clear(&chunk);
				image_clear(image);
				return mistake;
			}
			decoding(image);
			break;
		default:
			if (feof(file) && end == 0)
			{
				fclose(file);
				chunk_clear(&chunk);
				image_clear(image);
				return ERROR_DATA_INVALID;
			}
			break;
		}
		chunk_clear(&chunk);
	}
	fclose(file);
	return mistake;
}

int write_to_PNM(Image *image, const char *outfile)
{
	FILE *file = fopen(outfile, "wb");
	if (NULL == file)
	{
		fprintf(stderr, "Can't open file");
		return ERROR_UNSUPPORTED;
	}
	int a;
	if (image->type_color == 3 || image->type_color == 2)
	{
		a = 3;
	}
	else
	{
		a = 1;
	}
	unsigned int size = image->image_width * image->image_height * a;
	fprintf(file, "P%d\n%d %d\n%d\n", (image->type == P6) ? 6 : 5, image->image_width, image->image_height, 255);
	if (fwrite(image->outPalet, 1, size, file) != size)
	{
		fprintf(stderr, "Can't write to the file");
		fclose(file);
		return ERROR_UNKNOWN;
	}

	fclose(file);
	return SUCCESS;
}

int main(int argc, char *argv[])
{
	if (argc != 3)
	{
		fprintf(stderr, "expected 2 arguments");
		return ERROR_PARAMETER_INVALID;
	}
	Image image = create_Image();
	int mistake = get_Image(argv[1], &image);
	if (mistake == SUCCESS)
	{
		mistake = write_to_PNM(&image, argv[2]);
	}
	image_clear(&image);
	free(image.outPalet);
	return mistake;
}
