/**
 * Bank implementation.
 *
 * @author : Korshunov Ilya
 */
import java.util.concurrent.locks.ReentrantReadWriteLock
class BankImpl(n: Int) : Bank {
    private val accounts: Array<Account> = Array(n) { Account() }

    override val numberOfAccounts: Int
        get() = accounts.size

    override fun getAmount(index: Int): Long {
        val curLock = accounts[index].lock.readLock()
        curLock.lock()
        try {
            return accounts[index].amount
        } finally {
            curLock.unlock()
        }
    }


    override val totalAmount: Long
        get() {
            val locks = accounts.indices.sorted().map { accounts[it].lock.readLock() }
            locks.forEach { it.lock() }
            try {
                return accounts.sumOf { it.amount }
            } finally {
                locks.asReversed().forEach { it.unlock() }
            }
        }


    override fun deposit(index: Int, amount: Long): Long {
        val curLock = accounts[index].lock.writeLock()
        curLock.lock()
        try {
            require(amount > 0) { "Invalid amount: $amount" }
            val account = accounts[index]
            check(amount <= Bank.MAX_AMOUNT && account.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }
            account.amount += amount
            return account.amount
        } finally {
            curLock.unlock()
        }

    }

    override fun withdraw(index: Int, amount: Long): Long {
        val curLock = accounts[index].lock.writeLock()
        curLock.lock()
        try {
            require(amount > 0) { "Invalid amount: $amount" }
            val account = accounts[index]
            check(account.amount - amount >= 0) { "Underflow" }
            account.amount -= amount
            return account.amount
        } finally {
            curLock.unlock()
        }
    }

    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        val first = fromIndex.coerceAtMost(toIndex)
        val second = toIndex.coerceAtLeast(fromIndex)
        val firstLock = accounts[first].lock.writeLock()
        val secondLock = accounts[second].lock.writeLock()
        firstLock.lock()
        secondLock.lock()
        try {
            require(amount > 0) { "Invalid amount: $amount" }
            require(fromIndex != toIndex) { "fromIndex == toIndex" }
            val from = accounts[fromIndex]
            val to = accounts[toIndex]
            check(amount <= from.amount) { "Underflow" }
            check(!(amount > Bank.MAX_AMOUNT || to.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }
            from.amount -= amount
            to.amount += amount
        } finally {
            secondLock.unlock()
            firstLock.unlock()
        }
    }

    override fun consolidate(fromIndices: List<Int>, toIndex: Int) {
        val sortedArray = (fromIndices + toIndex).sorted()
        val sortedLocks = sortedArray.map { accounts[it].lock.writeLock() }
        sortedLocks.forEach { it.lock() }
        try {
            require(fromIndices.isNotEmpty()) { "empty fromIndices" }
            require(fromIndices.distinct() == fromIndices) { "duplicates in fromIndices" }
            require(toIndex !in fromIndices) { "toIndex in fromIndices" }
            val fromList = fromIndices.map { accounts[it] }
            val to = accounts[toIndex]
            val amount = fromList.sumOf { it.amount }
            check(to.amount + amount <= Bank.MAX_AMOUNT) { "Overflow" }
            for (from in fromList) from.amount = 0
            to.amount += amount
        } finally {
            sortedLocks.asReversed().forEach { it.unlock() }
        }
    }

    class Account {
        val lock = ReentrantReadWriteLock()
        var amount: Long = 0
    }
}