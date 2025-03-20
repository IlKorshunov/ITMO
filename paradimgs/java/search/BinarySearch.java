package search;

public class BinarySearch {
    // Pred for func: для любого i > j : arr[i] > arr[j]
    // Примечание: Нам не обязательно, чтобы x0 принадлежал отрезку
    public static int binary_search(int[] arr, int x0) {
        // Pred: true
        int left = -1;
        int right = arr.length;
        // Post =: left = -1 && right = arr.length
        //
        //
        // Pred for while: I:  left < x0 <= right
        // && (left' > left || right' < right) &&
        // main Inv: arr[left] > arr[right] (следование из предусловие func)
        while (right - left > 1) { // cond
            // right - left > 1 (cond)  && Inv -> left < mid < right
            // && arr[left] > arr[right]
            // Pred for =: left < mid < right
            int mid = left + (right - left) / 2;
            //  Post: mid' = left + (right - left) / 2 && right > left && left < mid' < right
            //
            // Pred for if : left < mid' < right
            // && arr[left] > arr[mid'] >= arr[right]; (from pred for funk)
            if (arr[mid] > x0) {
                //Pred for =: arr[mid'] > x0 && left < mid' < right && arr[left] > arr[mid'] >= arr[right]
                left = mid;
                // right - left' >= 1 && arr[left'] > arr[mid] >= arr[right] && пропуск ненужных элементов
            } else {
                // Pred for =: arr[mid'] <= x0 && right > mid' > left && arr[left] > arr[mid'] >= arr[right]
                right = mid;
                // right' - left >= 1 && arr[left'] > arr[mid] >= arr[right'] && пропуск ненужных элементов
            }
            // Post for if: right' - left' >= 1 && arr[left'] > arr[mid] >= arr[right'] && Inv
            // Post for if -> Inv (arr[left] > arr[right])
        }
        // Post for while: right' - left' = 1 && right > -1 && left < arr.length && arr[left'] > x0 >= arr[right']
        // (поскольку right = left + 1 && массив отсортирован && arr[left] > x0)
        // Post for while -> post for func
        return right;
    }
    // Post for func: right - min i : arr[i] <= x && (right - left = 1) && right > -1 && left < arr.length


    // Pred for func: arr- отсортированный массив && arr[left] > arr[right] &&
    // left < x0 <= right
    // && (left' > left || right' < right) &&
    //  main Inv: arr[left] > arr[right] (следование из предусловие func)
    public static int recbinary_search(int[] arr, int x0, int left, int right){

        // Pred :  right > -1 && left < arr.length && arr[left] > arr[mid] >= arr[right]
        if (right - left == 1){
            return right;
        }
        // Post : right - left != 1 && right >= 0 && left < arr.length && arr[left] > arr[mid] >= arr[right]

        // Pred: left < mid < right
        int mid = left + (right - left) / 2;
        // Post: left < mid' < right
        // Post for mid -> Pred for if
        // Pred for if : left < mid' < right && arr[left] > arr[mid'] >= arr[right]
        if (arr[mid] <= x0){
            // Pred : arr[mid] <= x0 && arr[left] > arr[mid'] >= arr[right]
           return recbinary_search(arr, x0, left, mid);
           // Post: left' = mid' && arr[left'] > arr[mid'] >= arr[right] && right' - left' >= 1
        } else {
            // Pred: arr[mid] > x0 && arr[left] > arr[mid'] >= arr[right]
           return recbinary_search(arr, x0, mid, right);
           // Post : right' = mid' && arr[right'] <= arr[mid'] < arr[left] && right' - left' >= 1
        }
        // Post for left' && right' -> Post for if
        // Post for if : right - left = 1 && arr[right'] <= arr[mid'] < arr[left]
        // Post for if -> Post for if -> Post for func
    }
    // Post for func: right - min i : arr[i] <= x && (right - left = 1 || (left = right && arr.size = 1))

    public static void main(String[] args) {
        // Pred : мы умеем переводить строковое представление числа в число
        int x = Integer.parseInt(args[0]);
        int[] arr = new int[args.length - 1];
        // Post : x = args[0] && arr = new int[args.length - 1]

        // Pred :  мы умеем переводить строковое представление числа в число
        for (int i = 1; i < args.length; i++) {
            arr[i - 1] = Integer.parseInt(args[i]);
        }
        // Post : мы заполнили массив

        // Pred : true
        System.out.println(recbinary_search(arr, x, -1, arr.length));
        // Post : выведли условие задачи
    }



}