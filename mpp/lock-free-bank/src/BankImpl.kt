import kotlinx.atomicfu.*

/**
 * @author Korshunov Ilya
 */

class BankImpl(override val numberOfAccounts: Int) : Bank {

    private val accounts = atomicArrayOfNulls<Account>(numberOfAccounts)

    init { for (i in 0 until numberOfAccounts) accounts[i].value = Account(0) }

    private fun account(index: Int) = accounts[index].value!!

    override fun getAmount(index: Int): Long {
        while (true) {
            val account = account(index)
            if (!account.invokeOperation()) return account.amount
        }
    }

    override val totalAmount: Long
        get() {
            val op = TotalAmountOp()
            op.invokeOperation()
            return op.sum
        }

    override fun deposit(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        check(amount <= MAX_AMOUNT) { "Overflow" }
        while (true) {
            val account = account(index)
            if (account.invokeOperation()) continue
            check(account.amount + amount <= MAX_AMOUNT) { "Overflow" }
            val updated = Account(account.amount + amount)
            if (accounts[index].compareAndSet(account, updated)) return updated.amount
        }
    }

    override fun withdraw(index: Int, amount: Long): Long {
        while (true) {
            val account = account(index)
            if (account.invokeOperation()) continue
            require(amount > 0) { "Invalid amount: $amount" }
            check(account.amount - amount >= 0) { "Underflow" }
            val updated = Account(account.amount - amount)
            if (accounts[index].compareAndSet(account, updated)) return updated.amount
        }
    }

    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromIndex != toIndex) { "fromIndex == toIndex" }
        check(amount <= MAX_AMOUNT) { "Underflow/overflow" }
        val op = TransferOp(fromIndex, toIndex, amount)
        op.invokeOperation()
        op.errorMessage?.let { error(it) }
    }


    private fun acquire(index: Int, op: Op): AcquiredAccount? {
        /*
         * This method must loop trying to replace accounts[index] with an instance of
         *     new AcquiredAccount(<old-amount>, op) until that successfully happens and return the
         *     instance of AcquiredAccount in this case.
         *
         *
         * Because accounts[index] does not have an ABA problem, there is no need to implement full-blown
         * DCSS operation with descriptors for DCSS operation as explained in Harris CASN work. A simple
         * lock-free compareAndSet loop suffices here if op.completed is checked after the accounts[index]
         * is read.
         *
         */
        while (true) {
            val account = account(index)
            if (account is AcquiredAccount && account.op == op) return account
            if (op.completed) return null
            account.invokeOperation()
            val acquiredAccount = AcquiredAccount(account.amount, op)
            if (accounts[index].compareAndSet(account, acquiredAccount)) {
                return acquiredAccount
            }
        }
    }

    private fun release(index: Int, op: Op) {
        assert(op.completed)
        val account = account(index)
        if (account is AcquiredAccount && account.op === op) {
            val updated = Account(account.newAmount)
            accounts[index].compareAndSet(account, updated)
        }
    }

    private open class Account(val amount: Long) {
        open fun invokeOperation(): Boolean = false
    }

    private class AcquiredAccount(
        var newAmount: Long, // New amount of funds in this account when op completes.
        val op: Op
    ) : Account(newAmount) {
        override fun invokeOperation(): Boolean {
            op.invokeOperation()
            return true
        }
    }

    private abstract inner class Op {
        @Volatile
        var completed = false

        abstract fun invokeOperation()
    }

    private inner class TotalAmountOp : Op() {
        var sum = 0L

        override fun invokeOperation() {
            var sum = 0L
            var acquired = 0
            while (acquired < numberOfAccounts) {
                val account = acquire(acquired, this) ?: break
                sum += account.newAmount
                acquired++
            }
            if (acquired == numberOfAccounts) {
                this.sum = sum
                completed = true
            }
            /*
             * To ensure lock-freedom, we must release all accounts even if this particular helper operation
             * had failed to acquire all of them before somebody else had completed the operations.
             * By releasing all accounts for completed operation we ensure progress of other operations.
             */
            for (i in 0 until numberOfAccounts) {
                release(i, this)
            }
        }
    }

    private inner class TransferOp(val fromIndex: Int, val toIndex: Int, val amount: Long) : Op() {
        var errorMessage: String? = null

        override fun invokeOperation() {
            val first = fromIndex.coerceAtMost(toIndex)
            val second = fromIndex.coerceAtLeast(toIndex)

            val acquredFirst = acquire(first, this)
            val acquredSecond = acquire(second, this)

            val flag = first == fromIndex

            val from = if (flag) acquredFirst else acquredSecond
            val to = if (!flag) acquredFirst else acquredSecond

            if (checkingNull(acquredFirst, acquredSecond, second, first) || checkingErrors(from, to, second, first)) return

            from!!.newAmount = from.amount - amount
            to!!.newAmount = to.amount + amount

            completed = true
            releasIng(second, first)
        }

        fun checkingNull(one : AcquiredAccount?, two : AcquiredAccount?, oneInd : Int, twoInd : Int) : Boolean {
            if (one == null || two == null) {
                releasIng(oneInd, twoInd)
                return true
            }
            return false
        }

        fun checkingErrors(from : AcquiredAccount?, to : AcquiredAccount?, oneInd : Int, twoInd : Int): Boolean {
            if (to!!.amount + amount > MAX_AMOUNT) errorMessage = "Overflow"
            if (amount > from!!.amount) errorMessage = "Underflow"
            val flag = errorMessage != null
            if (flag) {
                completed = true
                releasIng(twoInd, oneInd)
            }
            return flag
        }

        fun releasIng(one : Int, two : Int) {
            release(one, this)
            release(two, this)
        }
    }
}