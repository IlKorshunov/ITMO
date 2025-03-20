#pragma once
#include <string>

class MyException
{
  public:
	int errorCode;
	std::string errorMessage;

	MyException(int code, std::string message) : errorCode(code), errorMessage(std::move(message)) {}
};