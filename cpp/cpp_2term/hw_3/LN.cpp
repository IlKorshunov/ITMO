#include "LN.h"

#include "MyException.h"
#include "return_codes.h"
#include <string_view>

#include <cstring>
#include <iostream>
#include <limits>
#include <utility>

//// constructors
LN::LN()
{
	digits.push_back(0);
	is_negative = false;
	is_NaN = false;
}

LN::LN(long long number)
{
	is_negative = number < 0;
	is_NaN = false;
	do
	{
		digits.push_back(number % dimension);
		number /= dimension;
	} while (number > 0);
}

LN::LN(const char* str)
{
	size_t i = strlen(str);
	if (str[i - 1] == '\r')
	{
		i--;
	}
	is_negative = false;
	if (str[0] == '-')
	{
		is_negative = true;
		i--;
		str += 1;
	}

	while (i > 0)
	{
		size_t chunk_size = std::min(i, static_cast< size_t >(8));
		std::string chunk(str + i - chunk_size, chunk_size);
		for (char c : chunk)
		{
			if (!std::isxdigit(static_cast< unsigned char >(c)))
			{
				throw MyException(ERROR_DATA_INVALID, "incorrect input data");
			}
		}
		uint32_t chunk_value = std::stoul(chunk, nullptr, 16);
		digits.push_back(chunk_value);
		i -= chunk_size;
	}

	is_NaN = false;
	DeleteZeroes();
}
LN::LN(std::string_view str) : LN(str.data()) {}

LN::LN(LN&& a)
{
	digits = std::move(a.digits);
	is_negative = a.is_negative;
	is_NaN = a.is_NaN;

	a.digits.clear();
	a.is_negative = false;
	a.is_NaN = false;
}
LN::LN(const LN& a)
{
	digits = a.digits;
	is_negative = a.is_negative;
	is_NaN = a.is_NaN;
}

LN::~LN()
{
	digits.clear();
	is_negative = false;
	is_NaN = false;
}

LN::operator long long()
{
	long long result = 0;
	long long digit_number = 1;
	if (is_NaN)
	{
		return result;
	}

	for (size_t i = 0; i < digits.size(); i++)
	{
		if (std::numeric_limits< long long >::max() / digit_number < dimension)
		{
			throw MyException(ERROR_UNSUPPORTED, "Error: Multiplication overflow occurred.");
		}
		digit_number *= dimension;

		result += static_cast< long long >(digits[i]) * digit_number;
		if (result > (std::numeric_limits< long long >::max() - (static_cast< long long >(digits[i]) * digit_number)))
		{
			throw MyException(ERROR_UNSUPPORTED, "Error: Number exceeds the value of long long.");
		}
	}
	if (is_negative)
	{
		result = -result;
	}
	return result;
}

/// helper

void LN::DeleteZeroes()
{
	if (is_NaN)
	{
		return;
	}
	while (digits.size() > 1 && digits.back() == 0)
	{
		digits.pop_back();
	}
	if (digits[0] == 0 && is_negative)
	{
		is_negative = false;
	}
}

LN SumAbs(const LN& a, const LN& b)
{
	LN result;
	if (a.is_NaN || b.is_NaN)
	{
		result.is_NaN = true;
		return result;
	}
	uint64_t carry = 0;
	auto newSize = std::max(a.digits.size(), b.digits.size());
	result.digits.resize(newSize, 0);

	for (int i = 0; i < newSize; ++i)
	{
		carry += (i < a.digits.size()) ? static_cast< uint64_t >(a.digits.at(i)) : 0;
		carry += (i < b.digits.size()) ? static_cast< uint64_t >(b.digits.at(i)) : 0;
		result.digits.at(i) = static_cast< uint32_t >(carry);
		carry >>= 32;
	}

	if (carry != 0)
	{
		result.digits.push_back(static_cast< uint32_t >(carry));
	}

	result.DeleteZeroes();
	return result;
}

LN DifAbs(const LN& a, const LN& b)
{
	LN result;
	if (a.is_NaN || b.is_NaN)
	{
		result.is_NaN = true;
		return result;
	}
	result.digits.resize(a.digits.size(), 0);
	uint32_t borrow = 0;

	for (size_t i = 0; i < a.digits.size(); ++i)
	{
		uint64_t diff = static_cast< uint64_t >(a.digits[i]) - borrow;
		if (i < b.digits.size())
		{
			diff -= b.digits[i];
		}
		borrow = (diff > a.digits[i]) || (i < b.digits.size() && diff > (a.digits[i] - b.digits[i]));
		result.digits[i] = static_cast< uint32_t >(diff);
	}

	result.DeleteZeroes();
	return result;
}

LN LN::helpdivision(const LN& divider) const
{
	LN out;
	if (divider == LN() || is_NaN || divider.is_NaN)
	{
		out.is_NaN = true;
		return out;
	}
	else
	{
		size_t newsize = this->digits.size() - divider.digits.size() + 1;
		out.digits.resize(newsize);
		for (size_t i = newsize; i > 0; i--)
		{
			uint64_t r = dimension;
			uint64_t l = 0;
			while (r - l > 1)
			{
				uint64_t mid = (r + l) / 2;
				out.digits.at(i - 1) = mid;
				LN stor = out;
				stor *= divider;
				if (Abs(stor) <= Abs(*this))
				{
					l = mid;
				}
				else
				{
					r = mid;
				}
			}
			out.digits.at(i - 1) = l;
		}
		out.DeleteZeroes();
		out.is_negative = this->is_negative ^ divider.is_negative;
	}
	return out;
}

LN Abs(const LN& a)
{
	if (a.is_NaN)
	{
		LN res;
		res.is_NaN = true;
		return res;
	}
	LN result = a;
	result.is_negative = false;
	return result;
}

int CompareLN(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN)
	{
		LN res;
		res.is_NaN = true;
		return res;
	}
	if (a.is_negative != b.is_negative)
	{
		return a.is_negative ? -1 : 1;
	}

	if (a.digits.size() != b.digits.size())
	{
		return (a.digits.size() < b.digits.size()) ^ (a.is_negative) ? -1 : 1;
	}

	for (int i = a.digits.size() - 1; i >= 0; --i)
	{
		if (a.digits[i] != b.digits[i])
		{
			return (a.digits[i] < b.digits[i]) ^ (a.is_negative) ? -1 : 1;
		}
	}
	return 0;
}

/// move and copy operators

LN& LN::operator=(LN&& a)
{
	if (this != &a)
	{
		digits = std::move(a.digits);
		is_negative = a.is_negative;
		is_NaN = a.is_NaN;

		a.digits.clear();
		a.is_negative = false;
		a.is_NaN = false;
	}
	return *this;
}

LN& LN::operator=(const LN& a)
{
	if (this != &a)
	{
		digits = a.digits;
		is_negative = a.is_negative;
		is_NaN = a.is_NaN;
	}
	return *this;
}

LN& LN::operator+=(const LN& a)
{
	if (is_NaN || a.is_NaN)
	{
		is_NaN = true;
		return *this;
	}
	if (this->is_negative == a.is_negative)
	{
		this->digits = SumAbs(*this, a).digits;
	}
	else
	{
		bool this_neg = is_negative;
		bool a_neg = a.is_negative;
		const bool abs_a_greater = (Abs(*this) < Abs(a));
		this->digits = DifAbs(abs_a_greater ? a : *this, abs_a_greater ? *this : a).digits;
		this->is_negative = abs_a_greater ? a_neg : this_neg;
	}
	DeleteZeroes();
	return *this;
}

LN LN::operator-() const
{
	LN result = *this;
	result.is_negative = !is_negative;
	result.is_NaN = is_NaN;
	result.DeleteZeroes();
	return result;
}

LN& LN::operator-=(const LN& a)
{
	if (a.is_NaN || this->is_NaN)
	{
		this->is_NaN = true;
		return *this;
	}
	*this += -a;
	DeleteZeroes();
	return *this;
}

LN& LN::operator*=(const LN& a)
{
	if (a.is_NaN || this->is_NaN)
	{
		this->is_NaN = true;
		return *this;
	}
	bool result_negative = (is_negative ^ a.is_negative);
	LN result;
	result.digits.resize(digits.size() + a.digits.size(), 0);
	for (size_t i = 0; i < digits.size(); ++i)
	{
		uint64_t carry = 0;
		for (size_t j = 0; j < a.digits.size(); ++j)
		{
			uint64_t cur = static_cast< uint64_t >(digits[i]) * static_cast< uint64_t >(a.digits[j]) + result.digits[i + j] + carry;
			carry = cur >> 32;
			result.digits[i + j] = cur & 0xFFFFFFFF;
		}
		result.digits[i + a.digits.size()] = carry;
	}

	result.is_negative = result_negative;
	result.DeleteZeroes();
	*this = std::move(result);
	return *this;
}

LN& LN::operator/=(const LN& second)
{
	if (second.is_NaN || this->is_NaN || second == LN())
	{
		this->is_NaN = true;
		return *this;
	}
	if (Abs(*this) < Abs(second))
	{
		*this = LN();
		return *this;
	}
	*this = std::move(helpdivision(second));
	return *this;
}

LN& LN::operator%=(const LN& a)
{
	if (a.is_NaN || this->is_NaN)
	{
		this->is_NaN = true;
		return *this;
	}
	*this -= (*this / a) * a;
	DeleteZeroes();
	return *this;
}

/// string operators

LN operator"" _ln(const char* str)
{
	if (str[0] == '0' && str[1] == 'x')
	{
		LN ans = LN(str + 2);
		return ans;
	}
	else
	{
		LN ans = LN(str);
		return ans;
	}
}

char* LN::to_string() const
{
	size_t size = digits.size() * 8 + 2;
	char* ans = (char*)malloc(sizeof(char) * size);
	if (ans == nullptr)
	{
		throw MyException(ERROR_OUT_OF_MEMORY, "Memory allocation failed.");
	}
	if (*this == LN())
	{
		ans[0] = '0';
		ans[1] = '\0';
		return ans;
	}
	memset(ans, '0', sizeof(char) * size);
	char* p = ans;

	if (is_negative)
	{
		*p = '-';
		++p;
	}

	for (size_t i = digits.size(); i != 0; --i)
	{
		char buffer[9];
		size_t l = snprintf(buffer, sizeof(buffer), "%X", digits[i - 1]);
		if (i == digits.size())
		{
			strncpy(p, buffer, l);
			p += l;
		}
		else
		{
			strncpy(p + 8 - l, buffer, l);
			p += 8;
		}
	}

	*p = '\0';

	return ans;
}

/// logical operators

bool operator==(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN)
	{
		return false;
	}
	return CompareLN(a, b) == 0;
}

bool operator<(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN)
	{
		return false;
	}
	return CompareLN(a, b) < 0;
}

bool operator>(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN)
	{
		return false;
	}
	return operator<(b, a);
}

bool operator!=(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN)
	{
		return true;
	}
	return !operator==(a, b);
}

bool operator<=(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN)
	{
		return false;
	}
	return !operator>(a, b);
}

bool operator>=(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN)
	{
		return false;
	}
	return !operator<(a, b);
}

//// arithmetic operations

LN operator+(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN)
	{
		LN res;
		res.is_NaN = true;
		return res;
	}
	LN result = a;
	result += b;
	return result;
}

LN operator-(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN)
	{
		LN res;
		res.is_NaN = true;
		return res;
	}
	LN result = a;
	result -= b;
	return result;
}

LN operator*(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN)
	{
		LN res;
		res.is_NaN = true;
		return res;
	}
	LN result = a;
	result *= b;
	return result;
}

LN operator/(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN || b == LN())
	{
		LN res;
		res.is_NaN = true;
		return res;
	}
	LN result = a;
	result /= b;
	return result;
}

LN operator%(const LN& a, const LN& b)
{
	if (a.is_NaN || b.is_NaN)
	{
		LN res;
		res.is_NaN = true;
		return res;
	}
	LN result = a;
	result %= b;
	result.DeleteZeroes();
	return result;
}

//// other operations

LN operator~(const LN& a)
{
	if (a.is_NaN || a < LN())
	{
		LN res;
		res.is_NaN = true;
		return res;
	}
	if (a == LN() || a == LN(1))
	{
		return a;
	}
	LN left = LN();
	LN right = a;
	LN mid;
	LN result;

	while (left <= right)
	{
		mid = (left + right) / LN(2);
		result = mid * mid;

		if (result == a)
		{
			return mid;
		}
		else if (result < a)
		{
			left = mid + LN(1);
		}
		else
		{
			right = mid - LN(1);
		}
	}

	return right;
}

LN::operator bool() const
{
	if (is_NaN)
	{
		return false;
	}
	return *this == LN();
}

//// modification

LN& LN::operator++()
{
	*this += LN(1);
	return *this;
}

LN& LN::operator--()
{
	*this -= LN(1);
	return *this;
}

LN LN::operator++(int)	 
{
	LN temp(*this);
	*this += LN(1);
	return temp;
}

LN LN::operator--(int)	   
{
	LN temp(*this);
	*this -= LN(1);
	return temp;
}

LN LN::operator+() const
{
	LN result = *this;
	result.is_negative = false;
	result.is_NaN = is_NaN;
	result.DeleteZeroes();
	return result;
}