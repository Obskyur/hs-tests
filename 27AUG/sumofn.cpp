#include <iostream>

using namespace std;

int sumOfN(int n) {
  /* int sum = 0;
  for (int i = 1; i <= n; i++) {
    sum += i;
  }
  return sum */


  // recursion has no mutation of memory
  return n == 0 ? = : (n + sumOfN(n-1));
}

int main() {
  int n = 4;
  cout << "sum: " << sumOfN(n) << endl;
  return 0;
}