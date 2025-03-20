import kotlin.random.Random
import kotlin.test.*

private const val N = 10

/**
 * Functional single-threaded test-suite for bank implementation.
 */
class FunctionalTest {
    private val bank: Bank = BankImpl(N)

    @Test
    fun testEmptyBank() {
        assertEquals(N, bank.numberOfAccounts)
        assertEquals(0, bank.totalAmount)
        for (i in 0 until N) assertEquals(0, bank.getAmount(i))
    }

    @Test
    fun testDeposit() {
        val amount = 1234L
        val result = bank.deposit(1, amount)
        assertEquals(amount, result)
        assertEquals(amount, bank.getAmount(1))
        assertEquals(amount, bank.totalAmount)
    }

    @Test
    fun testWithdraw() {
        val depositAmount = 2345L
        val depositResult = bank.deposit(1, depositAmount)
        assertEquals(depositAmount, depositResult)
        assertEquals(depositAmount, bank.getAmount(1))
        assertEquals(depositAmount, bank.totalAmount)
        val withdrawAmount = 1234L
        val withdrawResult = bank.withdraw(1, withdrawAmount)
        assertEquals(depositAmount - withdrawAmount, withdrawResult)
        assertEquals(depositAmount - withdrawAmount, bank.getAmount(1))
        assertEquals(depositAmount - withdrawAmount, bank.totalAmount)
    }

    @Test
    fun testTotalAmount() {
        val deposit1 = 4567L
        val depositResult1 = bank.deposit(1, deposit1)
        assertEquals(deposit1, depositResult1)
        assertEquals(deposit1, bank.totalAmount)
        val deposit2 = 6789L
        val depositResult2 = bank.deposit(2, deposit2)
        assertEquals(deposit2, depositResult2)
        assertEquals(deposit2, bank.getAmount(2))
        assertEquals(deposit1 + deposit2, bank.totalAmount)
    }

    @Test
    fun testTransfer() {
        val depositAmount = 9876L
        val depositResult = bank.deposit(1, depositAmount)
        assertEquals(depositAmount, depositResult)
        assertEquals(depositAmount, bank.getAmount(1))
        assertEquals(depositAmount, bank.totalAmount)
        val transferAmount = 5432L
        bank.transfer(1, 2, transferAmount)
        assertEquals(depositAmount - transferAmount, bank.getAmount(1))
        assertEquals(transferAmount, bank.getAmount(2))
        assertEquals(depositAmount, bank.totalAmount)
    }

    @Test
    fun testConsolidate() {
        val depositAmount1 = 4823L
        val depositResult1 = bank.deposit(1, depositAmount1)
        assertEquals(depositAmount1, depositResult1)
        assertEquals(depositAmount1, bank.getAmount(1))
        assertEquals(depositAmount1, bank.totalAmount)
        val depositAmount2 = 4234L
        val depositResult2 = bank.deposit(2, depositAmount2)
        assertEquals(depositAmount2, depositResult2)
        assertEquals(depositAmount2, bank.getAmount(2))
        assertEquals(depositAmount1 + depositAmount2, bank.totalAmount)
        val depositAmount3 = 3131L
        val depositResult3 = bank.deposit(3, depositAmount3)
        assertEquals(depositAmount3, depositResult3)
        assertEquals(depositAmount3, bank.getAmount(3))
        assertEquals(depositAmount1 + depositAmount2 + depositResult3, bank.totalAmount)
        bank.consolidate(listOf(1, 2), 4)
        assertEquals(0L, bank.getAmount(1))
        assertEquals(0L, bank.getAmount(2))
        assertEquals(depositAmount1 + depositAmount2, bank.getAmount(4))
        assertEquals(depositAmount1 + depositAmount2 + depositResult3, bank.totalAmount)
        bank.consolidate(listOf(3), 4)
        assertEquals(0L, bank.getAmount(3))
        assertEquals(depositAmount1 + depositAmount2 + depositResult3, bank.getAmount(4))
        assertEquals(depositAmount1 + depositAmount2 + depositResult3, bank.totalAmount)
    }

    @Test
    fun testModelEquivalence() {
        val n = 10
        val bank = BankImpl(n)
        val model = BankImplModel(n)
        val rnd = Random(1)
        repeat(100_000) {
            val operation = Operation.entries.random<Operation>(rnd)
            val opBlock: (Bank) -> Any = when (operation) {
                Operation.getAmount -> {
                    val i = rnd.nextInt(n)
                    wrap { it.getAmount(i) }
                }
                Operation.totalAmount -> {
                    wrap { it.totalAmount }
                }
                Operation.deposit -> {
                    val i = rnd.nextInt(n)
                    val amount = rnd.nextLong(Bank.MAX_AMOUNT * 2) - Bank.MAX_AMOUNT / 2
                    wrap { it.deposit(i, amount) }
                }
                Operation.withdraw -> {
                    val i = rnd.nextInt(n)
                    val amount = rnd.nextLong(Bank.MAX_AMOUNT * 2) - Bank.MAX_AMOUNT / 2
                    wrap { it.withdraw(i, amount) }
                }
                Operation.transfer -> {
                    val i = rnd.nextInt(n)
                    val j = rnd.nextInt(n)
                    val amount = rnd.nextLong(Bank.MAX_AMOUNT * 2) - Bank.MAX_AMOUNT / 2
                    wrap { it.transfer(i, j, amount) }
                }
                Operation.consolidate -> {
                    val k = rnd.nextInt(n)
                    val i = (1..k).map { rnd.nextInt(n) }
                    val j = rnd.nextInt(n)
                    wrap { it.consolidate(i, j) }
                }
            }
            val expected = opBlock(model)
            val actual = opBlock(bank)
            assertEquals(expected, actual, "Result after operation #${it + 1} $operation")
        }
    }

    fun wrap(action: (Bank) -> Any): (Bank) -> Any {
        return {
            try {
                action(it)
            } catch (e: IllegalStateException) {
                "IllegalStateException"
            } catch (e: IllegalArgumentException) {
                "IllegalArgumentException"
            }
        }
    }

    enum class Operation { getAmount, totalAmount, deposit, withdraw, transfer, consolidate }
}