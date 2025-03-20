/**
 * @author Korshunov Ilya
 */

import java.util.concurrent.locks.ReentrantLock

class BankImpl(n: Int) : Bank {
    private val accounts: Array<Account> = Array(n) { Account() }

    override val numberOfAccounts: Int
        get() = accounts.size

    override fun getAmount(index: Int): Long {
        accounts[index].lock.lock()
        try {
            return accounts[index].amount
        } finally {
            accounts[index].lock.unlock()
        }
    }

    override val totalAmount: Long
        get() {
            accounts.forEach { it.lock.lock() }
            try {
                return accounts.sumOf { account -> account.amount }
            } finally {
                accounts.forEach { it.lock.unlock() }
            }
        }

    override fun deposit(index: Int, amount: Long): Long {
        accounts[index].lock.lock()
        try {
            require(amount > 0) { "Invalid amount: $amount" }
            val account = accounts[index]
            check(!(amount > Bank.MAX_AMOUNT || account.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }
            account.amount += amount
            return account.amount
        } finally {
            accounts[index].lock.unlock()
        }
    }

    override fun withdraw(index: Int, amount: Long): Long {
        accounts[index].lock.lock()
        try {
            require(amount > 0) { "Invalid amount: $amount" }
            val account = accounts[index]
            check(account.amount - amount >= 0) { "Underflow" }
            account.amount -= amount
            return account.amount
        } finally {
            accounts[index].lock.unlock()
        }
    }

    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        val first = fromIndex.coerceAtMost(toIndex)
        val second = toIndex.coerceAtLeast(fromIndex)

        accounts[first].lock.lock()
        accounts[second].lock.lock()
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
            accounts[second].lock.unlock()
            accounts[first].lock.unlock()
        }
    }

    class Account {
        val lock: ReentrantLock = ReentrantLock()
        var amount: Long = 0
    }
}
