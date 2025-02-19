#include <iostream>
#include <stack>
using namespace std;

// Partition function from textbook implementations
int partition(int arr[], int low, int high) {
    int pivot = arr[high]; // Pivot element
    int i = low - 1; // Index for the smaller element
    
    for (int j = low; j < high; j++) {
        if (arr[j] <= pivot) {
            i++;
            swap(arr[i], arr[j]);
        }
    }
    swap(arr[i + 1], arr[high]);
    return i + 1; // Return the partition index
}

// Non-recursive Quicksort using a stack
void quicksortIterative(int arr[], int low, int high) {
    stack<pair<int, int>> s;
    s.push({low, high});
    
    while (!s.empty()) {
        pair<int, int> range = s.top();
        int low = range.first;
        int high = range.second;
        s.pop();
        
        if (low < high) {
            int pivot = partition(arr, low, high);
            
            // Push left subarray
            if (pivot - 1 > low)
                s.push({low, pivot - 1});
            
            // Push right subarray
            if (pivot + 1 < high)
                s.push({pivot + 1, high});
        }
    }
}

// Helper function to print an array
void printArray(int arr[], int size) {
    for (int i = 0; i < size; i++)
        cout << arr[i] << " ";
    cout << endl;
}

int main() {
    int arr[] = {10, 7, 8, 3, 9, 1, 5, 4};
    int n = sizeof(arr) / sizeof(arr[0]);
    
    cout << "Original array: ";
    printArray(arr, n);
    
    quicksortIterative(arr, 0, n - 1);
    
    cout << "Sorted array: ";
    printArray(arr, n);
    
    return 0;
}
