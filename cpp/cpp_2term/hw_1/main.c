#include "return_codes.h"

#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct Matrix create(size_t rows, size_t cols, int *flag);
void init_matrix(struct Matrix *mat, int *flag);
double *readData(char *filename, int *const n, int *flag);
struct Matrix create(size_t rows, size_t cols, int *flag);
void copy_matrix(struct Matrix *mat, double *in, int *flag);
void free_memory(struct Matrix *mat, int *flag);
double *get_indicator(const struct Matrix *mat, size_t i, size_t j, int *flag);
struct Matrix matrix_multiply(struct Matrix *mat1, struct Matrix *mat2, int *flag);
double complex(const struct Matrix *mat, int *flag);
struct Matrix transpose_matrix(struct Matrix *mat, int *flag);
double *readData(char *filename, int *const n, int *flag);
void print_matrix(const struct Matrix *mat, int *flag);

struct Matrix
{
	double *data;
	size_t rows;
	size_t cols;
};

struct Matrix create(size_t rows, size_t cols, int *flag)
{
	struct Matrix result;
	result.rows = rows;
	result.cols = cols;

	result.data = (double *)malloc(sizeof(double) * rows * cols);
	if (result.data == NULL)
	{
		*flag = ERROR_OUT_OF_MEMORY;
	}
	return result;
}

void init_matrix(struct Matrix *mat, int *flag)
{
	if (mat == NULL || mat->data == NULL)
	{
		*flag = ERROR_UNKNOWN;
		return;
	}
	for (size_t i = 0; i < mat->rows * mat->cols; i++)
	{
		(*mat).data[i] = 0;
	}
}

void copy_matrix(struct Matrix *mat, double *in, int *flag)
{
	(*mat).data = in;
}

void free_memory(struct Matrix *mat, int *flag)
{
	if (mat == NULL)
	{
		*flag = ERROR_UNKNOWN;
		return;
	}
	if (mat->data == NULL)
	{
		*flag = ERROR_UNKNOWN;
		return;
	}
	free(mat->data);
	mat->data = NULL;
	*flag = SUCCESS;
}

double *get_indicator(const struct Matrix *mat, size_t i, size_t j,
					  int *flag)	// returns the memory address
{
	if (mat == NULL)
	{
		*flag = ERROR_UNKNOWN;
		return NULL;
	}
	if (i >= mat->rows || j >= mat->cols)
	{
		*flag = ERROR_UNKNOWN;
		return NULL;
	}
	return (*mat).data + i * (*mat).cols + j;
}

struct Matrix matrix_multiply(struct Matrix *mat1, struct Matrix *mat2, int *flag)
{
	if (mat1->cols != mat2->rows)
	{
		*flag = ERROR_UNKNOWN;
	}
	struct Matrix result;
	if (*flag == SUCCESS)
	{
		result = create((*mat1).rows, (*mat2).cols, flag);
	}

	if (result.data == NULL)
	{
		*flag = ERROR_OUT_OF_MEMORY;
	}

	if (*flag == SUCCESS)
	{
		for (size_t i = 0; i < (*mat1).rows; i++)
		{
			for (size_t j = 0; j < (*mat2).cols; j++)
			{
				double sum = 0.0;
				for (size_t k = 0; k < (*mat1).cols; k++)
				{
					sum += (*mat1).data[i * (*mat1).cols + k] * (*mat2).data[k * (*mat2).cols + j];
				}
				*get_indicator(&result, i, j, flag) = sum;
			}
		}
	}
	return result;
}

struct Matrix transpose_matrix(struct Matrix *mat, int *flag)
{
	if (mat->cols != mat->rows)
	{
		*flag = ERROR_UNKNOWN;
	}
	struct Matrix temp;
	if (*flag == SUCCESS)
	{
		temp = create((*mat).rows, (*mat).cols, flag);
	}

	if (temp.data == NULL)
	{
		*flag = ERROR_OUT_OF_MEMORY;
	}
	if (*flag == SUCCESS)
	{
		init_matrix(&temp, flag);
	}
	for (size_t i = 0; i < (*mat).rows; i++)
	{
		for (size_t j = 0; j < (*mat).cols; j++)
		{
			double now = *get_indicator(mat, i, j, flag);
			*get_indicator(&temp, j, i, flag) = now;
		}
	}

	size_t temp_rows = (*mat).rows;
	(*mat).rows = (*mat).cols;
	(*mat).cols = temp_rows;
	return temp;
}

struct Matrix QR_DEC(struct Matrix *mat, int *flag)
{
	struct Matrix Q, Q_T, R;
	if (*flag == SUCCESS)
	{
		Q = create(mat->rows, mat->cols, flag);
	}

	if (Q.data == NULL)
	{
		*flag = ERROR_OUT_OF_MEMORY;
	}

	// calculating the v_j column
	for (size_t j = 0; j < mat->cols; j++)
	{
		struct Matrix v_j = create(mat->rows, 1, flag);
		if (v_j.data == NULL)
		{
			*flag = ERROR_OUT_OF_MEMORY;
		}

		if (*flag == SUCCESS)
		{
			for (size_t i = 0; i < mat->rows; i++)
			{
				*get_indicator(&v_j, i, 0, flag) = *get_indicator(mat, i, j, flag);
			}
		}

		for (size_t k = 0; k < j; k++)
		{
			struct Matrix qk = create(Q.rows, 1, flag);
			if (qk.data == NULL)
			{
				*flag = ERROR_OUT_OF_MEMORY;
			}

			if (*flag == SUCCESS)
			{
				for (size_t i = 0; i < Q.rows; i++)
				{
					*get_indicator(&qk, i, 0, flag) = *get_indicator(&Q, i, k, flag);
				}

				double dot_product = 0.0;
				for (size_t i = 0; i < mat->rows; i++)
				{
					dot_product += *get_indicator(&v_j, i, 0, flag) * *get_indicator(&qk, i, 0, flag);
				}

				for (size_t i = 0; i < mat->rows; i++)
				{
					*get_indicator(&v_j, i, 0, flag) -= dot_product * *get_indicator(&qk, i, 0, flag);
				}

				free_memory(&qk, flag);
			}
		}

		// Normalization of column v
		double v_norm = 0.0;
		for (size_t i = 0; i < mat->rows; i++)
		{
			v_norm += *get_indicator(&v_j, i, 0, flag) * *get_indicator(&v_j, i, 0, flag);
		}
		v_norm = sqrt(v_norm);

		for (size_t i = 0; i < mat->rows; i++)
		{
			*get_indicator(&Q, i, j, flag) = *get_indicator(&v_j, i, 0, flag) / v_norm;
		}
		free_memory(&v_j, flag);
	}

	if (*flag == SUCCESS)
	{
		Q_T = transpose_matrix(&Q, flag);
	}

	if (*flag == SUCCESS)
	{
		R = matrix_multiply(&Q_T, mat, flag);
	}

	struct Matrix res;

	if (*flag == SUCCESS)
	{
		res = matrix_multiply(&R, &Q, flag);
	}
	free_memory(&Q, flag);
	free_memory(&Q_T, flag);
	free_memory(&R, flag);
	return res;
}

double
	real(const struct Matrix *mat, int *flag)
{
	double ans = (*get_indicator(mat, 0, 0, flag) + *get_indicator(mat, 1, 1, flag)) / 2;
	return ans;
}

double complex(const struct Matrix *mat, int *flag)
{
	double in_sqrt1 = (*get_indicator(mat, 0, 0, flag) + *get_indicator(mat, 1, 1, flag)) / 2;
	in_sqrt1 *= in_sqrt1;
	double in_sqrt2 =
		(*get_indicator(mat, 0, 0, flag) * *get_indicator(mat, 1, 1, flag) -
		 *get_indicator(mat, 0, 1, flag) * *get_indicator(mat, 1, 0, flag));
	return in_sqrt1 - in_sqrt2;
}

bool is_complex(const struct Matrix *mat, int *flag)
{
	bool a = (complex(mat, flag) < 0) ? true : false;
	return a;
}

void take_value(struct Matrix *one, const struct Matrix *two, int i, int j)
{
	(*one).data[0] = (*two).data[i * (*two).cols + j];
	(*one).data[1] = (*two).data[i * (*two).cols + (j + 1)];
	(*one).data[2] = (*two).data[(i + 1) * (*two).cols + j];
	(*one).data[3] = (*two).data[(i + 1) * (*two).cols + (j + 1)];
	// printf("%f %f %f %f\n", (*one).data[0], (*one).data[1], (*one).data[2],
	// (*one).data[3]);
}

struct Matrix Shura(struct Matrix *first, int *flag, int n)
{
	for (int i = 0; i < 10000; i++)
	{
		if (*flag == SUCCESS)
		{
			struct Matrix old = *first;
			*first = QR_DEC(first, flag);
			free_memory(&old, flag);
		}
		else
		{
			break;
		}
	}
	return *first;
}

void get_ans(char *filename, struct Matrix *in, int *flag)
{
	FILE *outputFile = fopen(filename, "w");
	if (outputFile == NULL)
	{
		*flag = ERROR_CANNOT_OPEN_FILE;
	}
	int j = 0;
	double prev = 0;
	for (int i = j; i < (*in).rows - 1; i++)
	{
		struct Matrix one = create(2, 2, flag);
		if (*flag == SUCCESS)
		{
			take_value(&one, in, i, j);
			if (is_complex(&one, flag))
			{
				fprintf(outputFile, "%lf %lfi\n", real(&one, flag), -sqrt(-complex(&one, flag)));
				fprintf(outputFile, "%lf %lfi\n", real(&one, flag), sqrt(-complex(&one, flag)));
				prev = 1;
			}
			else
			{
				if (prev != 1)
				{
					fprintf(outputFile, "%lf\n", *get_indicator(&one, 0, 0, flag));
					prev = 0;
				}
				else
				{
					prev = 0;
				}
			}
			j++;
			free_memory(&one, flag);
		}
	}
	if (prev == 0 && *flag == SUCCESS)
	{
		fprintf(outputFile, "%lf\n", *get_indicator(in, (in)->cols - 1, (in)->cols - 1, flag));
	}
	if (fclose(outputFile) != 0)
	{
		*flag = ERROR_UNKNOWN;
	}
}

double *readData(char *filename, int *const n, int *flag)
{
	*flag = SUCCESS;
	FILE *inputFile = fopen(filename, "r");
	if (fscanf(inputFile, "%d", n) != 1)
	{
		*flag = ERROR_CANNOT_OPEN_FILE;
		return NULL;
	}
	double *matrix = (double(*))malloc(sizeof(double) * *n * *n);
	if (matrix == NULL)
	{
		*flag = ERROR_DATA_INVALID;
		return NULL;
	}
	for (int i = 0; i < *n; i++)
	{
		for (int j = 0; j < *n; j++)
		{
			if (fscanf(inputFile, "%lf", &matrix[i * *n + j]) != 1)
			{
				*flag = ERROR_DATA_INVALID;
				return NULL;
			}
		}
	}
	if (fclose(inputFile) != 0)
	{
		*flag = ERROR_UNKNOWN;
	}
	return matrix;
}

void print_matrix(const struct Matrix *mat, int *flag)
{	 // for debugging
	for (size_t i = 0; i < (*mat).rows; i++)
	{
		for (size_t j = 0; j < (*mat).cols; j++)
		{
			printf("%f ", *get_indicator(mat, i, j, flag));
		}
		printf("\n");
	}
	printf("\n");
}

int main(int argc, char **argv)
{
	if (argc != 3)
	{
		return ERROR_PARAMETER_INVALID;
	}
	int n;
	int flag = SUCCESS;
	double *matrix1 = readData(argv[1], &n, &flag);
	struct Matrix my;
	my.rows = my.cols = n;
	if (flag == SUCCESS)
	{
		copy_matrix(&my, matrix1, &flag);
	}

	struct Matrix ans;
	if (flag == SUCCESS)
	{
		ans = Shura(&my, &flag, n);
	}
	if (flag == SUCCESS)
	{
		get_ans(argv[2], &ans, &flag);
	}
	free_memory(&ans, &flag);

	return flag;
}