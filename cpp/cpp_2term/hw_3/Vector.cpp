#include "Vector.h"

#include "MyException.h"
#include "return_codes.h"

Vector::Vector() = default;
Vector::Vector(const Vector &other) : Vector(other, other._capacity) {}
Vector::Vector(const Vector &other, size_t new_capacity)
{
	_data = (uint32_t *)malloc(new_capacity * sizeof(uint32_t));
	if (_data == nullptr)
	{
		throw MyException(ERROR_OUT_OF_MEMORY, "Memory allocation failed.");
	}
	if (!other.empty())
		memcpy(_data, other._data, other._size * sizeof(uint32_t));
	_size = other._size;
	_capacity = new_capacity;
}
Vector::~Vector()
{
	free(_data);
}
Vector &Vector::operator=(const Vector &other)
{
	Vector(other).swap(*this);
	return *this;
}

void Vector::swap(Vector &other)
{
	size_t temp = other._size;
	other._size = _size;
	_size = temp;

	temp = other._capacity;
	other._capacity = _capacity;
	_capacity = temp;

	uint32_t *tdata = other._data;
	other._data = _data;
	_data = tdata;
}
