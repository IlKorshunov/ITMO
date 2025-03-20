package search;

public class BinarySearchMax {
    // Pred Pred for func: для любого i > j : arr[i] > arr[j], а также циклически сдвинут на величину k>=0, те k элементов
    // были взяты с конца и перествалены в начало
    public static int itebinary_search(int[] arr) {
        // Pred: true
        int left = -1;
        int right = arr.length;
        // post : left = -1 && right = arr.length

        // Pred : left = -1 && right = arr.length
        if (arr.length == 1) {
            // Pred : left = -1 && right = arr.length && arr.length = 1
            return arr[0];
            // Post : arr[0] - max
        }
        // Post : arr.length > 1

        // Pred : arr.length > 1
        if (arr[arr.length - 1] > arr[0]) {
            // Pred : arr[arr.length-1] > arr[0] && arr.length > 1
            return arr[arr.length - 1];
            // Post : k = 0 -> arr[arr.length-1] - max
        }
        // Post : величина сдвига > 0 && arr[arr.length-1] < arr[0]

        // Pred : right > left && right > -1 && left < arr.length && (left' > left || right' < right) &&
        // arr[left] > arr[right]
        while (right - left > 1) {
            // Pred : Inv && right - left > 1
            int mid = left + (right - left) / 2;
            // Post : mid' = left + (right - left) / 2 && right > mid > left
            if (arr[mid] > arr[arr.length - 1]) {
                // Pred : arr[mid] > arr[arr.length-1]
                left = mid;
                // Post : right - left >= 1 && arr[left] > arr[right]
            } else {
                // Pred : arr[mid] < arr[arr.length-1] && right - left > 1 && right > mid > left
                right = mid;
                // Post : right - left >= 1 && arr[left] > arr[right]
            }
            // Post : right - left >= 1 && arr[left] > arr[right]
        }
        // Post : right' - left' = 1 && arr[left] > arr[left - 1] > arr[left+1]

        return arr[left];
    }
    // Post: arr[left] - максимальное значение массива

    // Pred : массив был отсортирован по возрастанию, а затем циклически сдвинут на величину >= 0
    public static int recbinary_search(int[] arr, int left, int right) {


        // Pred : left = -1 && right = arr.length
        if (arr.length == 1) {
            // Pred : left = -1 && right = arr.length && arr.length = 1
            return arr[0];
            // Post : arr[0] - max
        }
        // Post : arr.length > 1

        // Pred : arr.length > 1
        if (arr[arr.length - 1] > arr[0]) {
            // Pred : arr[arr.length-1] > arr[0] && arr.length > 1
            return arr[arr.length - 1];
            // Post : arr[arr.length-1] - max
        }
        // Post : величина сдвига > 0 && arr[arr.length-1] < arr[0]

        // величина сдвига > 0 &&  arr.length > 1 && arr[left] > arr[right]
        if (right - left == 1) {
            // right - left ==1 && arr[left] > arr[right]
            return arr[left];
            // Post : arr[left] - max
        }
        // Post : right - left > 1

        int mid = left + (right - left) / 2;
        // post : mid' = left + (right - left) / 2 && arr[left] >  arr[right]

        if (arr[mid] > arr[arr.length - 1]) {
            // Pred : arr[left] >  arr[right] && arr[mid] > arr[arr.length-1]
            return recbinary_search(arr, mid, right);
            // Post : left = mid' && arr[left] >  arr[right] && right - left >= 1
        } else {
            // pred : arr[left] >  arr[right] && arr[mid] < arr[arr.length-1]
            return recbinary_search(arr, left, mid);
            // Post : right = mid' && arr[left] >  arr[right] && right - left >= 1
        }
    }
    // Post : arr[left] - максимальное значение массива

    public static void main(String[] args) {
        // Pred : мы умеем переводить строковое представление числа в число
        int[] arr = new int[args.length];
        int sum = 0;
        // Post : sum = 0 && rr = new int[args.length]

        // Pred : мы умеем переводить строковое представление числа в число
        for (int i = 0; i < args.length; i++) {
            arr[i] = Integer.parseInt(args[i]);
            sum += Integer.parseInt(args[i]);
        }
        // Post : мы заполним массив и посчитаем сумму.

        // Pred : мы посчитали сумму и умеем выводить двумя способами
        if (sum % 2 == 0) {
            // sum % 2 == 0
            System.out.println(itebinary_search(arr));
            // Post : itebinary_search(arr). вывели максимум.
        } else {
            // sum % 2 != 0
            System.out.println(recbinary_search(arr, -1, arr.length));
            // Post : recbinary_search(arr, -1, arr.length). вывели максимум
        }
        // post : Если сумма всех чисел во входе чётная, то должна быть использоваться рекурсивная версия, иначе — итеративная.
    }
}

