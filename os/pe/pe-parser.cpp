#include <iostream>
#include <fstream>
#include <cstring>

size_t read(std::ifstream &pe, size_t start, size_t count)
{
    pe.seekg(start, pe.beg);
    size_t res = 0;
    pe.read((char *)&res, count);
    return res;
}

std::string read_dll_name(std::ifstream &pe, size_t adr)
{
    pe.seekg(adr);
    std::string dll_name;
    std::getline(pe, dll_name, '\0');
    return dll_name;
}

uint32_t from_rva(uint32_t count, uint32_t start_section_headers, uint32_t rva, std::ifstream &file)
{
    for (size_t i = 0; i < count; ++i)
    {
        uint32_t section_virtual_size = read(file, start_section_headers + 0x8 + i * 40, sizeof(uint32_t));
        uint32_t section_rva = read(file, start_section_headers + 0xC + i * 40, sizeof(uint32_t));
        uint32_t section_raw = read(file, start_section_headers + 0x14 + i * 40, sizeof(uint32_t));

        if (rva >= section_rva && rva < (section_rva + section_virtual_size))
        {
            uint32_t import_raw = section_raw + rva - section_rva;
            return import_raw;
        }
    }
    return 0;
}

void print_import_functions(std::ifstream &file, uint32_t start_section_headers, uint32_t count, uint32_t import_lookup_value)
{
    while (true)
    {
        uint64_t lookup_value_read = read(file, import_lookup_value, 8);

        if (lookup_value_read & 0x8000000000000000)
        {
            import_lookup_value += 8;
            continue;
        }

        if (lookup_value_read == 0)
        {
            break;
        }

        uint32_t name_value_rva = lookup_value_read & 0x000000007FFFFFFF;
        uint32_t name_from_rva = from_rva(count, start_section_headers, name_value_rva, file);

        std::string function_name = read_dll_name(file, name_from_rva + 2);
        std::cout << "    " << function_name << std::endl;

        import_lookup_value += 8;
    }
}

void print_import_table(std::ifstream &file, uint32_t start_section_headers, size_t count, uint32_t import_raw)
{
    while (true)
    {
        uint32_t name_rva = read(file, import_raw + 0xC, sizeof(uint32_t));

        if (name_rva == 0)
        {
            break;
        }

        uint32_t name_from_rva = from_rva(count, start_section_headers, name_rva, file);
        std::string dll_name = read_dll_name(file, name_from_rva);
        std::cout << dll_name << std::endl;

        uint32_t import_lookup_table_rva = read(file, import_raw, sizeof(uint32_t));
        uint32_t import_lookup_table_value = from_rva(count, start_section_headers, import_lookup_table_rva, file);
        print_import_functions(file, start_section_headers, count, import_lookup_table_value);
        import_raw += 20;
    }
}

bool is_pe_file(std::ifstream &file)
{
    uint32_t pe_offset = read(file, 0x3C, sizeof(pe_offset));
    file.seekg(pe_offset);

    char pe_signature[5];
    file.read(pe_signature, 4);
    pe_signature[4] = '\0';

    return (memcmp(pe_signature, "PE\0\0", 4) == 0);
}

int main(int argc, char *argv[])
{
    std::ifstream file(argv[2], std::ios::binary);

    if (strcmp(argv[1], "is-pe") == 0)
    {
        bool isPE = is_pe_file(file);
        if (isPE)
        {
            std::cout << "PE" << std::endl;
            return 0;
        }
        else
        {
            std::cout << "Not PE" << std::endl;
            return 1;
        }
    }
    else if (strcmp(argv[1], "import-functions") == 0)
    {
        uint32_t pe_offset = read(file, 0x3C, sizeof(pe_offset));
        size_t count = read(file, pe_offset + 6, 2); // количество секций в section_headers
        uint32_t start_opt_header = pe_offset + 24;
        uint32_t start_section_headers = start_opt_header + 240;

        uint32_t import_table_rva = read(file, start_opt_header + 0x78, 4);
        // uint32_t size_import_table = read(file, start_opt_header + 0x78 + 4, 4);

        uint32_t import_raw = from_rva(count, start_section_headers, import_table_rva, file);
        print_import_table(file, start_section_headers, count, import_raw);

        return 0;
    }

    file.close();
    return 0;
}
