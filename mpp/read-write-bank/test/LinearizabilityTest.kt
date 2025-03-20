import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.annotations.Param
import org.jetbrains.kotlinx.lincheck.check
import org.jetbrains.kotlinx.lincheck.paramgen.IntGen
import org.jetbrains.kotlinx.lincheck.paramgen.LongGen
import org.jetbrains.kotlinx.lincheck.strategy.stress.StressCTest
import org.jetbrains.kotlinx.lincheck.strategy.stress.StressOptions
import org.junit.Test

private const val N_ACCOUNTS = 5

/**
 * This test checks bank implementation for linearizability.
 */
class LinearizabilityTest() {
    @Param.Params(
        Param(name = "id", gen = IntGen::class, conf = "0:4"),
        Param(name = "amount", gen = LongGen::class, conf = "1:100")
    )
    @StressCTest
    open class BankOperations @JvmOverloads constructor(val bank: Bank = BankImpl(N_ACCOUNTS)) {
        @Operation(params = ["id"])
        fun getAmount(id: Int): Long =
            bank.getAmount(id)

        @get:Operation
        val totalAmount: Long
            get() = bank.totalAmount

        @Operation(params = ["id", "amount"])
        fun deposit(id: Int, amount: Long) =
            bank.deposit(id, amount)

        @Operation(params = ["id", "amount"])
        fun withdraw(id: Int, amount: Long) =
            bank.withdraw(id, amount)

        @Operation(params = ["id", "id", "amount"])
        fun transfer(idFrom: Int, idTo: Int, amount: Long) =
            bank.transfer(idFrom, idTo, amount)

        @Operation(params = ["id", "id"])
        fun consolidate1(idFrom1: Int, idTo: Int) =
            bank.consolidate(listOf(idFrom1), idTo)

        @Operation(params = ["id", "id", "id"])
        fun consolidate2(idFrom1: Int, idFrom2: Int, idTo: Int) =
            bank.consolidate(listOf(idFrom1, idFrom2), idTo)
    }

    class SequentialSpec : BankOperations(BankImplModel(N_ACCOUNTS))

    @Test
    fun test() {
        StressOptions()
            .sequentialSpecification(SequentialSpec::class.java)
            .check(BankOperations::class.java)
    }
}
