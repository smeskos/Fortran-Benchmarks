// Compile with:
// g++-10 -O3 -march=native -ffast-math -funroll-loops test.cpp
#include <iostream>

int main()
{
    int i = 0;
    long int s = 0;
    for (int i = 0; i < 1000000000; ++i)
      if (i % 2 == 0) s += i;
    std::cout << s << std::endl;
}