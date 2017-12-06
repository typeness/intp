# intp
Interpreter for dummy programming language.

Bubble sort example
```
bubbleSort = func(array) {
  i = 0
  while (i < size(array)) {
    j = 1
    while (j < size(array)) {
      if (array[j] < array[j - 1]) {
        t = array[j]
        array[j] = array[j - 1]
        array[j - 1] = t
      }
      j = j + 1
    }
    i = i + 1
  }
  return array
}

unsorted = [1, 5, 0, 4, 9, 2, 3, 8, 7, 6]
sorted = bubbleSort(unsorted)
println(sorted) // prints [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```
