import java.lang.reflect.Modifier
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.concurrent.locks.ReentrantReadWriteLock.ReadLock
import java.util.concurrent.locks.ReentrantReadWriteLock.WriteLock
import kotlin.random.Random
import kotlin.random.nextLong
import kotlin.test.*

class CodeTest {
    private val implClass = BankImpl::class.java
    private val accountClass = BankImpl.Account::class.java
    private val accountArrayClass = Array<BankImpl.Account>::class.java
    private val reentrantReadWriteLockClass = ReentrantReadWriteLock::class.java

    @Test
    fun testNoSynchronizedMethods() {
        for (m in implClass.declaredMethods) {
            assertFalse((m.modifiers and Modifier.SYNCHRONIZED) != 0, "BankImpl cannot use 'synchronized' methods")
        }
    }

    @Test
    fun testImplFields() {
        val f = implClass.declaredFields
        assertEquals(1, f.size, "BankImpl must have one field")
        assertEquals("accounts", f[0].name, "BankImpl must have accounts field")
        assertEquals(accountArrayClass, f[0].type, "BankImpl must have Account<Accounts> field")
    }

    @Test
    fun testAccountLock() {
        val lockField = accountClass.getDeclaredField("lock")
        assertEquals(reentrantReadWriteLockClass, lockField.type, "BankImpl.Account.lock field must have ReentrantReadWriteLock type")
    }

    @Suppress("UNCHECKED_CAST")
    @Test
    fun testFineGrainedReadWriteLock() {
        val n = 7
        val bank = loggedBankImpl(n)
        fun testAmount(i: Int) = 10L * (i + 1)
        fun checkTotal(expected: Long) {
            lockLog.clear()
            val total = bank.totalAmount
            assertEquals(expected, total)
            assertTrue(lockLog.all { it.contains("read.") }, "Expected only read locks on totalAmount")
        }
        var expectedTotal = 0L
        for (i in 0 until n) {
            lockLog.clear()
            bank.deposit(i, testAmount(i))
            expectedTotal += testAmount(i)
            assertEquals(listOf("write.lock($i)", "write.unlock($i)"), lockLog, "Expected write lock and unlock on deposit($i, ...)")
        }
        for (i in 0 until n) {
            lockLog.clear()
            val amount = bank.getAmount(i)
            assertEquals(testAmount(i), amount)
            assertEquals(listOf("read.lock($i)", "read.unlock($i)"), lockLog, "Expected read lock and unlock on getAmount($i)")
        }
        checkTotal(expectedTotal)
        for (i in 0 until n) {
            lockLog.clear()
            val result = bank.withdraw(i, testAmount(i))
            assertEquals(0, result, "Expected zero result after withdraw")
            assertEquals(listOf("write.lock($i)", "write.unlock($i)"), lockLog, "Expected write lock and unlock on withdraw($i, ...)")

        }
        checkTotal(0L)
    }

    @Test
    fun testFineGrainedConsolidateLock() {
        val n = 15
        val bank = loggedBankImpl(n)
        val rnd = Random(1)
        repeat(10) {
            val k = rnd.nextInt(1, n - 1)
            val shuffled = (0..<n).shuffled(rnd)
            val from = shuffled.take(k)
            val to = shuffled[k]
            var expectedTotal = 0L
            for (i in from) {
                lockLog.clear()
                val testAmount = rnd.nextLong(1..10000L)
                bank.deposit(i, testAmount)
                expectedTotal += testAmount
                assertEquals(listOf("write.lock($i)", "write.unlock($i)"), lockLog, "Expected write lock and unlock on deposit($i, ...)")
            }
            lockLog.clear()
            bank.consolidate(from, to)
            val all = from + to
            for (i in all) {
                assertTrue("write.lock($i)" in lockLog, "Expected write lock on consolidate with $i")
                assertTrue("write.unlock($i)" in lockLog, "Expected write unlock on consolidate with $i")
            }
            assertEquals(all.size * 2, lockLog.size, "Expected a total of 2 * ${all.size} lock/unlock operation on consolidate")
            lockLog.clear()
            val result = bank.withdraw(to, expectedTotal)
            assertEquals(0, result, "Expected zero result after withdraw")
            assertEquals(listOf("write.lock($to)", "write.unlock($to)"), lockLog, "Expected write lock and unlock on withdraw($to, ...)")
        }
    }

    private fun loggedBankImpl(n: Int): BankImpl {
        val bank = BankImpl(n)
        @Suppress("UNCHECKED_CAST")
        val accounts =
            implClass.getDeclaredField("accounts").apply { isAccessible = true }.get(bank) as Array<BankImpl.Account>
        val lockField = accountClass.getDeclaredField("lock").apply { isAccessible = true }
        for ((i, account) in accounts.withIndex()) {
            lockField.set(account, LoggedLock(i))
        }
        return bank
    }

    private val lockLog = ArrayList<String>()

    private inner class LoggedLock(val i: Int) : ReentrantReadWriteLock() {
        override fun readLock(): ReadLock {
            return LoggedReadLock(this)
        }

        override fun writeLock(): WriteLock {
            return LoggedWriteLock(this)
        }
    }

    private inner class LoggedReadLock(lock : LoggedLock) : ReadLock(lock) {
        private val i = lock.i

        override fun lock() {
            lockLog += "read.lock($i)"
            super.lock()
        }

        override fun unlock() {
            lockLog += "read.unlock($i)"
            super.unlock()
        }
    }

    private inner class LoggedWriteLock(lock : LoggedLock) : WriteLock(lock) {
        private val i = lock.i

        override fun lock() {
            lockLog += "write.lock($i)"
            super.lock()
        }

        override fun unlock() {
            lockLog += "write.unlock($i)"
            super.unlock()
        }
    }
}