#include "LN.h"
#include "MyException.h"
#include "return_codes.h"
#include <string_view>

#include <iostream>
#include <map>
#include <sstream>
#include <stack>
#include <string>

int Dekstra(const std::string& inputFile, const std::string& outputFile, const std::string& order)
{
	std::ifstream input(inputFile);
	std::ofstream output(outputFile);
	std::stack< LN > stack;
	std::string line;

	if (!input.is_open())
	{
		std::cerr << "Failed to open the input file." << std::endl;
		return ERROR_CANNOT_OPEN_FILE;
	}

	if (!output.is_open())
	{
		std::cerr << "Failed to open the output file." << std::endl;
		return ERROR_CANNOT_OPEN_FILE;
	}

	std::map< std::string, LN (*)(const LN&, const LN&) > binary_operations;
	binary_operations.emplace("+", [](const LN& a, const LN& b) { return a + b; });
	binary_operations.emplace("-", [](const LN& a, const LN& b) { return a - b; });
	binary_operations.emplace("*", [](const LN& a, const LN& b) { return a * b; });
	binary_operations.emplace("/", [](const LN& a, const LN& b) { return a / b; });
	binary_operations.emplace("==", [](const LN& a, const LN& b) { return LN(a == b); });
	binary_operations.emplace(">", [](const LN& a, const LN& b) { return LN(a > b); });
	binary_operations.emplace(">=", [](const LN& a, const LN& b) { return LN(a >= b); });
	binary_operations.emplace("<=", [](const LN& a, const LN& b) { return LN(a <= b); });
	binary_operations.emplace("!=", [](const LN& a, const LN& b) { return LN(a != b); });
	binary_operations.emplace("<", [](const LN& a, const LN& b) { return LN(a < b); });
	binary_operations.emplace("%", [](const LN& a, const LN& b) { return a % b; });

	std::map< std::string, LN (*)(const LN&) > unary_operations;
	unary_operations.emplace("~", [](const LN& a) { return ~a; });
	unary_operations.emplace("_", [](const LN& a) { return -a; });
	unary_operations.emplace(
		"postfix++",
		[](const LN& a) -> LN
		{
			LN copy(a);
			copy++;
			return a;
		});
	unary_operations.emplace(
		"postfix--",
		[](const LN& a) -> LN
		{
			LN copy(a);
			copy--;
			return a;
		});
	unary_operations.emplace(
		"++",
		[](const LN& a) -> LN
		{
			LN copy(a);
			return ++copy;
		});
	unary_operations.emplace(
		"--",
		[](const LN& a) -> LN
		{
			LN copy(a);
			return --copy;
		});
	unary_operations.emplace("__", [](const LN& a) { return +a; });

	while (std::getline(input, line))
	{
		auto op = binary_operations.find(line);
		if (op != binary_operations.end())
		{
			if (stack.size() < 2)
			{
				std::cerr << "Invalid expression: insufficient operands." << std::endl;
				return ERROR_DATA_INVALID;
			}

			LN operand1;
			LN operand2;

			if (order == "direct")
			{
				operand1 = stack.top();
				stack.pop();
				operand2 = stack.top();
				stack.pop();
			}
			else if (order == "inverse")
			{
				operand2 = stack.top();
				stack.pop();
				operand1 = stack.top();
				stack.pop();
			}

			stack.push(op->second(operand1, operand2));
			continue;
		}

		auto uop = unary_operations.find(line);
		if (uop != unary_operations.end())
		{
			if (stack.size() < 1)
			{
				std::cerr << "Invalid expression: insufficient operands." << std::endl;
				return ERROR_DATA_INVALID;
			}

			LN a = uop->second(stack.top());
			stack.pop();
			stack.push(a);
			continue;
		}
		if (line == "")
		{
			break;
		}
		LN in(line.c_str());
		stack.emplace(in);
	}

	while (!stack.empty())	  // I don't remember if we had to throw an error in the case of NaN, so when I found it, I
							  // went further.
	{
		if (stack.top().is_NaN)
		{
			output << "NaN" << std::endl;
			stack.pop();
			std::cerr << "You have received a NaN. Most likely, this is due to division by zero." << std::endl;
			continue;
		}
		char* str = stack.top().to_string();
		output << str << std::endl;
		free(str);
		stack.pop();
	}

	return SUCCESS;
}

int main(int argc, char* argv[])
{
	if (argc != 4)
	{
		fprintf(stderr, "Usage: %s <inputFile> <outputFile> <order>\n", argv[0]);
		return ERROR_PARAMETER_INVALID;
	}

	const std::string input = argv[1];
	const std::string output = argv[2];
	const std::string order = argv[3];

	if (order != "direct" && order != "inverse")
	{
		fprintf(stderr, "Invalid order. Use 'direct' or 'inverse'.\n");
		return ERROR_PARAMETER_INVALID;
	}

	int result = SUCCESS;
	try
	{
		result = Dekstra(input, output, order);
	} catch (const MyException& ex)
	{
		std::cerr << "Error: " << ex.errorMessage << " (code: " << ex.errorCode << ")" << std::endl;
		return ex.errorCode;
	}
	return result;
}
