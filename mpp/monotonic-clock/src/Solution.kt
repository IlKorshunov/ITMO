/**
 * @author Korshunov Ilya
 */
class Solution : MonotonicClock {
    private var c1 by RegularInt(0)
    private var c2 by RegularInt(0)
    private var c3 by RegularInt(0)

    private var copyC1 by RegularInt(0)
    private var copyC2 by RegularInt(0)
    private var copyC3 by RegularInt(0)
    override fun write(time: Time) {
        copyC1 = time.d1
        copyC2 = time.d2
        copyC3 = time.d3

        c3 = copyC3
        c2 = copyC2
        c1 = copyC1
    }

    override fun read(): Time {
        val r1 = c1
        val r2 = c2
        val r3 = c3

        val copyR3 = copyC3
        val copyR2 = copyC2
        val copyR1 = copyC1

        if (r1 == copyR1 && r2 == copyR2 && r3 == copyR3) {
            return Time(copyR1, copyR2, copyR3)
        } else {
            val time = arrayOf(r1, r2, r3)
            val copyTime = arrayOf(copyR1, copyR2, copyR3)
            for (i in time.indices) {
                if (time[i] != copyTime[i]) {
                    val out = arrayOf(0, 0, 0)
                    for (j in 0..i) {
                        out[j] = time[j]
                    }
                    out[i] = copyTime[i]
                    return Time(out[0], out[1], out[2])
                }
            }
        }
        return Time(0,0,0) // impossible return
    }
}