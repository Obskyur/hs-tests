const numbers = Array.from(Array(100).keys(), (i) => ++i);

console.log(numbers);

function odd(number) {
  // console.log("Called: ", number);
  return number % 2 !== 0;
}

console.log(odd(1)) // true

function getOddNumbers(n) {
  return numbers
    .filter(odd)
    .slice(0, n);
}
console.log(getOddNumbers());