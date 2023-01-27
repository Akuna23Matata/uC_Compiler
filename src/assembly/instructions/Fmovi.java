package assembly.instructions;

/**
 * Class corresponding to RISC-V NEG instruction
 * 
 * Models: neg dest src #dest = -1 * src
 */
public class Fmovi extends Instruction {

    /**
     * Initializes a FMOVI instruction that will print: NEG dest src
     * 
     * @param src source operand 1
     * @param dest destination operand
     */

    public Fmovi(String src, String dest) {
        super();
        this.src1 = src;
        this.dest = dest;
        this.oc = OpCode.FMOVI;
    }

    /**
     * @return "NEG dest src"
     */
    public String toString() {
        return this.oc + " " + this.dest + ", " + this.src1;
    }
}