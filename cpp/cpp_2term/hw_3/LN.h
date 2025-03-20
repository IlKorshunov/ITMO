#pragma once
#include "Vector.h"
#include <string_view>

#include <fstream>
#include <string>

class LN
{
  public:
	static const uint32_t dimension = 4294967295;
	Vector digits;
	bool is_NaN;
	bool is_negative;
	void DeleteZeroes();

	LN();
	LN(long long number);
	LN(const char* str);
	LN(std::string_view str);
	LN(LN&& a);
	LN(const LN& a);
	~LN();

	explicit operator long long();

	friend LN SumAbs(const LN& a, const LN& b);
	friend LN DifAbs(const LN& a, const LN& b);
	LN helpdivision(const LN& that) const;
	friend LN Abs(const LN& a);
	int CompareLN(const LN& a, const LN& b);

	LN& operator=(LN&& a);
	LN& operator=(const LN& a);

	friend bool operator<(const LN& a, const LN& b);
	friend bool operator>(const LN& a, const LN& b);
	friend bool operator==(const LN& a, const LN& b);
	friend bool operator!=(const LN& a, const LN& b);
	friend bool operator<=(const LN& a, const LN& b);
	friend bool operator>=(const LN& a, const LN& b);
	operator bool() const;

	LN& operator+=(const LN& a);
	LN operator-() const;
	LN operator+() const;
	LN& operator-=(const LN& a);
	LN& operator*=(const LN& a);
	LN& operator/=(const LN& a);
	LN& operator%=(const LN& a);

	friend LN operator~(const LN& a);
	char* to_string() const;

	friend LN operator+(const LN& a, const LN& b);
	friend LN operator-(const LN& a, const LN& b);
	friend LN operator*(const LN& a, const LN& b);
	friend LN operator/(const LN& a, const LN& b);
	friend LN operator%(const LN& a, const LN& b);

	LN& operator++();
	LN& operator--();
	LN operator++(int);
	LN operator--(int);
};

LN operator"" _ln(const char* str);
