/**
 * Bank implementation. Single-threaded version.
 */
class BankImplModel(n: Int) : Bank {
    private val accounts: Array<Account> = Array(n) { Account() }

    override val numberOfAccounts: Int
        get() = accounts.size

    override fun getAmount(index: Int): Long {
        return accounts[index].amount
    }

    override val totalAmount: Long
        get() {
            return accounts.sumOf { account ->
                account.amount
            }
        }

    override fun deposit(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[index]
        check(amount <= Bank.MAX_AMOUNT && account.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }
        account.amount += amount
        return account.amount
    }

    override fun withdraw(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[index]
        check(account.amount - amount >= 0) { "Underflow" }
        account.amount -= amount
        return account.amount
    }

    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromIndex != toIndex) { "fromIndex == toIndex" }
        val from = accounts[fromIndex]
        val to = accounts[toIndex]
        check(amount <= from.amount) { "Underflow" }
        check(amount <= Bank.MAX_AMOUNT && to.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }
        from.amount -= amount
        to.amount += amount
    }

    override fun consolidate(fromIndices: List<Int>, toIndex: Int) {
        require(fromIndices.isNotEmpty()) { "empty fromIndices" }
        require(fromIndices.distinct() == fromIndices) { "duplicates in fromIndices" }
        require(toIndex !in fromIndices) { "toIndex in fromIndices" }
        val fromList = fromIndices.map { accounts[it] }
        val to = accounts[toIndex]
        val amount = fromList.sumOf { it.amount }
        check(to.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }
        for (from in fromList) from.amount = 0
        to.amount += amount
    }

    class Account {
        /**
         * Amount of funds in this account.
         */
        var amount: Long = 0
    }
}