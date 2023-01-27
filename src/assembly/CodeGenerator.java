package assembly;

import java.util.List;

import compiler.Scope.SymbolTableEntry;
import ast.visitor.AbstractASTVisitor;

import ast.*;
import assembly.instructions.*;
import compiler.Scope;

public class CodeGenerator extends AbstractASTVisitor<CodeObject> {

	int intRegCount;
	int floatRegCount;
	static final public char intTempPrefix = 't';
	static final public char floatTempPrefix = 'f';
	
	int loopLabel;
	int elseLabel;
	int outLabel;

	String currFunc;
	
	public CodeGenerator() {
		loopLabel = 0;
		elseLabel = 0;
		outLabel = 0;
		intRegCount = 0;		
		floatRegCount = 0;
	}

	public int getIntRegCount() {
		return intRegCount;
	}

	public int getFloatRegCount() {
		return floatRegCount;
	}
	
	/**
	 * Generate code for Variables
	 * 
	 * Create a code object that just holds a variable
	 * 
	 * Important: add a pointer from the code object to the symbol table entry
	 *            so we know how to generate code for it later (we'll need to find
	 *            the address)
	 * 
	 * Mark the code object as holding a variable, and also as an lval
	 */
	@Override
	protected CodeObject postprocess(VarNode node) {
		
		Scope.SymbolTableEntry sym = node.getSymbol();
		
		CodeObject co = new CodeObject(sym);
		co.lval = true;
		co.type = node.getType();

		return co;
	}

	/** Generate code for IntLiterals
	 * 
	 * Use load immediate instruction to do this.
	 */
	@Override
	protected CodeObject postprocess(IntLitNode node) {
		CodeObject co = new CodeObject();
		
		//Load an immediate into a register
		//The li and la instructions are the same, but it's helpful to distinguish
		//for readability purposes.
		//li tmp' value
		Instruction i = new Li(generateTemp(Scope.Type.INT), node.getVal());

		co.code.add(i); //add this instruction to the code object
		co.lval = false; //co holds an rval -- data
		co.temp = i.getDest(); //temp is in destination of li
		co.type = node.getType();

		return co;
	}

	/** Generate code for FloatLiteras
	 * 
	 * Use load immediate instruction to do this.
	 */
	@Override
	protected CodeObject postprocess(FloatLitNode node) {
		CodeObject co = new CodeObject();
		
		//Load an immediate into a regisster
		//The li and la instructions are the same, but it's helpful to distinguish
		//for readability purposes.
		//li tmp' value
		Instruction i = new FImm(generateTemp(Scope.Type.FLOAT), node.getVal());

		co.code.add(i); //add this instruction to the code object
		co.lval = false; //co holds an rval -- data
		co.temp = i.getDest(); //temp is in destination of li
		co.type = node.getType();

		return co;
	}

	/**
	 * Generate code for binary operations.
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from left child
	 * Step 1a: if left child is an lval, add a load to get the data
	 * Step 2: add code from right child
	 * Step 2a: if right child is an lval, add a load to get the data
	 * Step 3: generate binary operation using temps from left and right
	 * 
	 * Don't forget to update the temp and lval fields of the code object!
	 * 	   Hint: where is the result stored? Is this data or an address?
	 * 
	 */
	@Override
	protected CodeObject postprocess(BinaryOpNode node, CodeObject left, CodeObject right) {

		CodeObject co = new CodeObject();
		
		//step 1
		// co.code.addAll(left.code);
		// System.out.println(left.getType());
		if (left.isVar()) {
			left.code.addAll(generateAddrFromVariable(left));
			left.temp = left.code.getLast().getDest();
		}
		co.code.addAll(left.code);
		if(left.lval){
			Instruction loadleft = null;
			if(left.getType() == Scope.Type.INT){
				loadleft = new Lw(generateTemp(left.getType()), left.temp, "0");
			}
			else if(left.getType() == Scope.Type.FLOAT){
				loadleft = new Flw(generateTemp(left.getType()), left.temp, "0");
			}
			else{
				loadleft = new Lw(generateTemp(left.getType()), left.temp, "0");
			}
			// System.out.println(left);
			// System.out.println(left.getType());
			left.temp = loadleft.getDest();
			co.code.add(loadleft);
		}
		//step 2
		// co.code.addAll(right.code);
		if (right.isVar()) {
			right.code.addAll(generateAddrFromVariable(right));
			right.temp = right.code.getLast().getDest();
		}
		co.code.addAll(right.code);
		if(right.lval){
			Instruction loadright = null;
			if(right.getType() == Scope.Type.INT){
				loadright = new Lw(generateTemp(right.getType()), right.temp, "0");
			}
			else if(right.getType() == Scope.Type.FLOAT){
				loadright = new Flw(generateTemp(right.getType()), right.temp, "0");
			}
			else{
				loadright = new Lw(generateTemp(right.getType()), right.temp, "0");
			}
			right.temp = loadright.getDest();
			co.code.add(loadright);
		}
		//step 3-0 type conversion
		if(left.getType() != right.getType()){
			//find which is int and convert to float
			if(left.getType() == Scope.Type.INT){
				co.code.add(new Imovf(left.temp, generateTemp(Scope.Type.FLOAT)));
				left.temp = co.code.getLast().getDest();
				left.type = Scope.Type.FLOAT;
			}
			else if(right.getType() == Scope.Type.INT){
				co.code.add(new Imovf(right.temp, generateTemp(Scope.Type.FLOAT)));
				right.temp = co.code.getLast().getDest();
				right.type = Scope.Type.FLOAT;
			}
		}
		//step 3
		Instruction bianryOp = null;
		if(node.getOp() == BinaryOpNode.OpType.ADD){
			if(left.getType() == Scope.Type.FLOAT){
				bianryOp = new FAdd(left.temp, right.temp, generateTemp(left.getType()));
			}
			else{
				bianryOp = new Add(left.temp, right.temp, generateTemp(left.getType()));
			}
		}
		else if(node.getOp() == BinaryOpNode.OpType.SUB){
			if(left.getType() == Scope.Type.FLOAT){
				bianryOp = new FSub(left.temp, right.temp, generateTemp(left.getType()));
			}
			else{
				bianryOp = new Sub(left.temp, right.temp, generateTemp(left.getType()));
			}
		}
		else if(node.getOp() == BinaryOpNode.OpType.MUL){
			if(left.getType() == Scope.Type.FLOAT){
				bianryOp = new FMul(left.temp, right.temp, generateTemp(left.getType()));
			}
			else{
				bianryOp = new Mul(left.temp, right.temp, generateTemp(left.getType()));
			}
		}
		else{
			if(left.getType() == Scope.Type.FLOAT){
				bianryOp = new FDiv(left.temp, right.temp, generateTemp(left.getType()));
			}
			else{
				
				bianryOp = new Div(left.temp, right.temp, generateTemp(left.getType()));
			}
		}
		//update values
		co.code.add(bianryOp);
		co.temp = bianryOp.getDest();
		co.type = left.getType();
		co.lval = false;
		return co;
	}

	/**
	 * Generate code for unary operations.
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from child expression
	 * Step 1a: if child is an lval, add a load to get the data
	 * Step 2: generate instruction to perform unary operation
	 * 
	 * Don't forget to update the temp and lval fields of the code object!
	 * 	   Hint: where is the result stored? Is this data or an address?
	 * 
	 */
	@Override
	protected CodeObject postprocess(UnaryOpNode node, CodeObject expr) {
		
		CodeObject co = new CodeObject();
		if (expr.isVar()) {
			expr.code.addAll(generateAddrFromVariable(expr));
			expr.temp = expr.code.getLast().getDest();
		}
		co.code.addAll(expr.code);
		if(expr.lval){
			Instruction loadexpr = null;
			if(expr.getType() == Scope.Type.FLOAT){
				loadexpr = new Flw(generateTemp(expr.getType()), expr.temp, "0");
			}
			else if(expr.getType() == Scope.Type.INT){
				loadexpr = new Lw(generateTemp(expr.getType()), expr.temp, "0");
				
			}
			expr.temp = loadexpr.getDest();
			co.code.add(loadexpr);
		}
		if(node.getOp() == UnaryOpNode.OpType.TOINT){
			if(expr.getType() == Scope.Type.FLOAT){
				co.code.add(new Fmovi(expr.temp, generateTemp(Scope.Type.INT)));
				co.temp = co.code.getLast().getDest();
				co.type = Scope.Type.INT;
				co.lval = false;
			}
			return co;
		}
		else if(node.getOp() == UnaryOpNode.OpType.TOFLOAT){
			if(expr.getType() == Scope.Type.INT){
				co.code.add(new Imovf(expr.temp, generateTemp(Scope.Type.FLOAT)));
				co.temp = co.code.getLast().getDest();
				co.type = Scope.Type.FLOAT;
				co.lval = false;
			}
			return co;
		}
		//step 2
		Instruction negat = null;
		if(expr.getType() == Scope.Type.INT){
			negat = new Neg(expr.temp, generateTemp(expr.getType()));
		}
		else if(expr.getType() == Scope.Type.FLOAT){
			negat = new FNeg(expr.temp, generateTemp(expr.getType()));
		}
		else{
			negat = new Neg(expr.temp, generateTemp(expr.getType()));
		}
		expr.temp = negat.getDest();
		co.code.add(negat);
		co.lval = false;
		co.temp = negat.getDest();
		return co;
	}

	/**
	 * Generate code for assignment statements
	 * 
	 * Step 0: create new code object
	 * Step 1: if LHS is a variable, generate a load instruction to get the address into a register
	 * Step 1a: add code from LHS of assignment (make sure it results in an lval!)
	 * Step 2: add code from RHS of assignment
	 * Step 2a: if right child is an lval, add a load to get the data
	 * Step 3: generate store
	 * 
	 * Hint: it is going to be easiest to just generate a store with a 0 immediate
	 * offset, and the complete store address in a register:
	 * 
	 * sw rhs 0(lhs)
	 */
	@Override
	protected CodeObject postprocess(AssignNode node, CodeObject left,
			CodeObject right) {
		
		CodeObject co = new CodeObject();
		if(right.temp == null){
			right.temp = right.code.getLast().getDest();
		}
		assert(left.lval == true); //left hand side had better hold an address
		//Step 1a
		if (left.isVar()) {
			left.code.addAll(generateAddrFromVariable(left));
			left.temp = left.code.getLast().getDest();
		}
		//step 1b
		co.code.addAll(left.code);
		//step 2
		if (right.isVar()) {
			System.out.println("Right is a var, is ERROR");
			right.code.addAll(generateAddrFromVariable(right));
			right.temp = right.code.getLast().getDest();
		}
		co.code.addAll(right.code);
		if(right.lval){
			Instruction loadright = null;
			if(right.getType() == Scope.Type.FLOAT){
				loadright = new Flw(generateTemp(right.getType()), right.temp, "0");
			}
			else{
				loadright = new Lw(generateTemp(right.getType()), right.temp, "0");
			}
			co.code.add(loadright);
			right.temp = loadright.getDest();
		}
		if(right.getType() != left.getType()){
			if(right.getType() == Scope.Type.INT){
				co.code.add(new Imovf(right.temp, generateTemp(Scope.Type.FLOAT)));
				right.temp = co.code.getLast().getDest();
				right.type = Scope.Type.FLOAT;
			}
			else if(right.getType() == Scope.Type.FLOAT){
				co.code.add(new Fmovi(right.temp, generateTemp(Scope.Type.INT)));
				right.temp = co.code.getLast().getDest();
				right.type = Scope.Type.INT;
			}
		}
		//step 3
		Instruction store = null;
		if(left.getType() == Scope.Type.FLOAT){
			store = new Fsw(right.temp, left.temp, "0");
		}
		else{
			store = new Sw(right.temp, left.temp, "0");
		}
		co.code.add(store);
		co.type = left.getType();
		co.lval = true;
		co.temp = left.temp;
		return co;
	}

	/**
	 * Add together all the lists of instructions generated by the children
	 */
	@Override
	protected CodeObject postprocess(StatementListNode node,
			List<CodeObject> statements) {
		CodeObject co = new CodeObject();
		//add the code from each individual statement
		for (CodeObject subcode : statements) {
			co.code.addAll(subcode.code);
		}
		co.type = null; //set to null to trigger errors
		return co;
	}
	
	/**
	 * Generate code for read
	 * 
	 * Step 0: create new code object
	 * Step 1: add code from VarNode (make sure it's an lval)
	 * Step 2: generate GetI instruction, storing into temp
	 * Step 3: generate store, to store temp in variable
	 */
	@Override
	protected CodeObject postprocess(ReadNode node, CodeObject var) {
		
		//Step 0
		CodeObject co = new CodeObject();

		//Generating code for read(id)
		assert(var.getSTE() != null); //var had better be a variable

		InstructionList il = new InstructionList();
		switch(node.getType()) {
			case INT: 
				//Code to generate if INT:
				//geti tmp
				//if var is global: la tmp', <var>; sw tmp 0(tmp')
				//if var is local: sw tmp offset(fp)
				Instruction geti = new GetI(generateTemp(Scope.Type.INT));
				il.add(geti);
				InstructionList store = new InstructionList();
				if (var.getSTE().isLocal()) {
					store.add(new Sw(geti.getDest(), "fp", String.valueOf(var.getSTE().addressToString())));
				} else {
					store.addAll(generateAddrFromVariable(var));
					store.add(new Sw(geti.getDest(), store.getLast().getDest(), "0"));
				}
				il.addAll(store);
				break;
			case FLOAT:
				//Code to generate if FLOAT:
				//getf tmp
				//if var is global: la tmp', <var>; fsw tmp 0(tmp')
				//if var is local: fsw tmp offset(fp)
				Instruction getf = new GetF(generateTemp(Scope.Type.FLOAT));
				il.add(getf);
				InstructionList fstore = new InstructionList();
				if (var.getSTE().isLocal()) {
					fstore.add(new Fsw(getf.getDest(), "fp", String.valueOf(var.getSTE().addressToString())));
				} else {
					fstore.addAll(generateAddrFromVariable(var));
					fstore.add(new Fsw(getf.getDest(), fstore.getLast().getDest(), "0"));
				}
				il.addAll(fstore);
				break;
			default:
				throw new Error("Shouldn't read into other variable");
		}
		
		co.code.addAll(il);

		co.lval = false; //doesn't matter
		co.temp = null; //set to null to trigger errors
		co.type = null; //set to null to trigger errors

		return co;
	}

	/**
	 * Generate code for print
	 * 
	 * Step 0: create new code object
	 * 
	 * If printing a string:
	 * Step 1: add code from expression to be printed (make sure it's an lval)
	 * Step 2: generate a PutS instruction printing the result of the expression
	 * 
	 * If printing an integer:
	 * Step 1: add code from the expression to be printed
	 * Step 1a: if it's an lval, generate a load to get the data
	 * Step 2: Generate PutI that prints the temporary holding the expression
	 */
	@Override
	protected CodeObject postprocess(WriteNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		//generating code for write(expr)

		//for strings, we expect a variable
		if (node.getWriteExpr().getType() == Scope.Type.STRING) {
			//Step 1:
			assert(expr.getSTE() != null);
			
			System.out.println("; generating code to print " + expr.getSTE());

			//Get the address of the variable
			InstructionList addrCo = generateAddrFromVariable(expr);
			co.code.addAll(addrCo);

			//Step 2:
			Instruction write = new PutS(addrCo.getLast().getDest());
			co.code.add(write);
		} else {
			//Step 1a:
			//if expr is an lval, load from it
			if (expr.lval == true) {
				expr = rvalify(expr);
			}
			
			//Step 1:
			co.code.addAll(expr.code);

			//Step 2:
			//if type of writenode is int, use puti, if float, use putf
			Instruction write = null;
			switch(node.getWriteExpr().getType()) {
			case STRING: throw new Error("Shouldn't have a STRING here");
			case INT: 
			case PTR: //should work the same way for pointers
				write = new PutI(expr.temp); break;
			case FLOAT: write = new PutF(expr.temp); break;
			default: throw new Error("WriteNode has a weird type");
			}

			co.code.add(write);
		}

		co.lval = false; //doesn't matter
		co.temp = null; //set to null to trigger errors
		co.type = null; //set to null to trigger errors

		return co;
	}

	/**
	 * FILL IN FROM STEP 3
	 * 
	 * Generating an instruction sequence for a conditional expression
	 * 
	 * Implement this however you like. One suggestion:
	 *
	 * Create the code for the left and right side of the conditional, but defer
	 * generating the branch until you process IfStatementNode or WhileNode (since you
	 * do not know the labels yet). Modify CodeObject so you can save the necessary
	 * information to generate the branch instruction in IfStatementNode or WhileNode
	 * 
	 * Alternate idea 1:
	 * 
	 * Don't do anything as part of CodeGenerator. Create a new visitor class
	 * that you invoke *within* your processing of IfStatementNode or WhileNode
	 * 
	 * Alternate idea 2:
	 * 
	 * Create the branch instruction in this function, then tweak it as necessary in
	 * IfStatementNode or WhileNode
	 * 
	 * Hint: you may need to preserve extra information in the returned CodeObject to
	 * make sure you know the type of branch code to generate (int vs float)
	 */
	@Override
	protected CodeObject postprocess(CondNode node, CodeObject left, CodeObject right) {
		CodeObject co = new CodeObject();

		/* FILL IN */
		//step 1
		// co.code.addAll(left.code);
		if (left.isVar()) {
			left.code.addAll(generateAddrFromVariable(left));
			left.temp = left.code.getLast().getDest();
		}
		co.code.addAll(left.code);
		if(left.lval){
			Instruction loadleft = null;
			if(left.getType() == Scope.Type.INT){
				loadleft = new Lw(generateTemp(left.getType()), left.temp, "0");
			}
			else if(left.getType() == Scope.Type.FLOAT){
				loadleft = new Flw(generateTemp(left.getType()), left.temp, "0");
			}
			else{
				loadleft = new Lw(generateTemp(left.getType()), left.temp, "0");
			}
			left.temp = loadleft.getDest();
			co.code.add(loadleft);
		}
		//step 2
		// co.code.addAll(right.code);
		if (right.isVar()) {
			right.code.addAll(generateAddrFromVariable(right));
			right.temp = right.code.getLast().getDest();
		}
		co.code.addAll(right.code);
		if(right.lval){
			Instruction loadright = null;
			if(right.getType() == Scope.Type.INT){
				loadright = new Lw(generateTemp(right.getType()), right.temp, "0");
			}
			else if(right.getType() == Scope.Type.FLOAT){
				loadright = new Flw(generateTemp(right.getType()), right.temp, "0");
			}
			else{
				loadright = new Lw(generateTemp(right.getType()), right.temp, "0");
			}
			right.temp = loadright.getDest();
			co.code.add(loadright);
		}
		//step 3
		//update values
		if(right.getType() == Scope.Type.INT){
	 		co.condType = node.getReversedOp().toString();
	 		co.firstVar = left.temp;
	 		co.secondVar = right.temp;
		}
		else{
			switch(node.getReversedOp()){
				case EQ: co.condType = "FEQ"; co.firstVar = left.temp; co.secondVar = right.temp; break;
				case NE: co.condType = "FNE"; co.firstVar = left.temp; co.secondVar = right.temp; break;
				case LT: co.condType = "FLT"; co.firstVar = left.temp; co.secondVar = right.temp; break;
				case LE: co.condType = "FLE"; co.firstVar = left.temp; co.secondVar = right.temp; break;
				case GT: co.condType = "FLT"; co.firstVar = right.temp; co.secondVar = left.temp; break;
				case GE: co.condType = "FLE"; co.firstVar = right.temp; co.secondVar = left.temp; break;
			}
		}
		return co;
	}

	/**
	 * FILL IN FROM STEP 3
	 * 
	 * Step 0: Create code object
	 * 
	 * Step 1: generate labels
	 * 
	 * Step 2: add code from conditional expression
	 * 
	 * Step 3: create branch statement (if not created as part of step 2)
	 * 			don't forget to generate correct branch based on type
	 * 
	 * Step 4: generate code
	 * 		<cond code>
	 *		<flipped branch> elseLabel
	 *		<then code>
	 *		j outLabel
	 *		elseLabel:
	 *		<else code>
	 *		outLabel:
	 *
	 * Step 5 insert code into code object in appropriate order.
	 */
	@Override
	protected CodeObject postprocess(IfStatementNode node, CodeObject cond, CodeObject tlist, CodeObject elist) {
		//Step 0:
		CodeObject co = new CodeObject();
		//Step 1: gen out label
		String outS = generateOutLabel();
		Label outL = new Label(outS);
		String elseS;
		Label elseL;
		if(elist.code.isEmpty()){
			elseS = outS;
			elseL = outL;
		}
		else{
			elseS = generateElseLabel();
			elseL = new Label(elseS);
		}
		//Step 2: add code from cond
		co.code.addAll(cond.code);
		Instruction jmp;
		String regi;
		switch(cond.condType){
			case "LT": jmp = new Blt(cond.firstVar, cond.secondVar, elseS); break;
			case "LE": jmp = new Ble(cond.firstVar, cond.secondVar, elseS); break;
			case "GT": jmp = new Bgt(cond.firstVar, cond.secondVar, elseS); break;
			case "GE": jmp = new Bge(cond.firstVar, cond.secondVar, elseS); break;
			case "EQ": jmp = new Beq(cond.firstVar, cond.secondVar, elseS); break;
			case "NE": jmp = new Bne(cond.firstVar, cond.secondVar, elseS); break;
			case "FEQ":
				regi = generateTemp(Scope.Type.INT);
				jmp = new Feq(cond.firstVar, cond.secondVar, regi);
				co.code.add(jmp);
				jmp = new Bne(regi, "x0", elseS);
				break;
			case "FNE":
				regi = generateTemp(Scope.Type.INT);
				jmp = new Feq(cond.firstVar, cond.secondVar, regi);
				co.code.add(jmp);
				jmp = new Beq(regi, "x0", elseS);
				break;
			case "FLT": 
				regi = generateTemp(Scope.Type.INT);
				jmp = new Flt(cond.firstVar, cond.secondVar, regi); 
				co.code.add(jmp);
				jmp = new Bne(regi, "x0", elseS);
				break;
			case "FLE":
				regi = generateTemp(Scope.Type.INT);
				jmp = new Fle(cond.firstVar, cond.secondVar, regi); 
				co.code.add(jmp);
				jmp = new Bne(regi, "x0", elseS);
				break;
			default: throw new Error("Cond have a weird type");
		}
		//Add all stmt
		co.code.add(jmp);
		co.code.addAll(tlist.code);
		//add else and out branch
		if(elist.code.isEmpty()){
			co.code.add(outL);
		}
		else{
			jmp = new J(outS);
			co.code.add(jmp);
			co.code.add(elseL);
			co.code.addAll(elist.code);
			co.code.add(outL);
		}
		return co;
	}

		/**
	 * FILL IN FROM STEP 3
	 * 
	 * Step 0: Create code object
	 * 
	 * Step 1: generate labels
	 * 
	 * Step 2: add code from conditional expression
	 * 
	 * Step 3: create branch statement (if not created as part of step 2)
	 * 			don't forget to generate correct branch based on type
	 * 
	 * Step 4: generate code
	 * 		loopLabel:
	 *		<cond code>
	 *		<flipped branch> outLabel
	 *		<body code>
	 *		j loopLabel
	 *		outLabel:
	 *
	 * Step 5 insert code into code object in appropriate order.
	 */
	@Override
	protected CodeObject postprocess(WhileNode node, CodeObject cond, CodeObject slist) {
		//Step 0:
		CodeObject co = new CodeObject();

		//labels
		String outS = generateOutLabel();
		Label outL = new Label(outS);
		String loopS = generateLoopLabel();
		Label loopL = new Label(loopS);
		//add codes:
		co.code.add(loopL);
		co.code.addAll(cond.code);
		Instruction jmp;
		String regi;
		switch(cond.condType){
			case "LT": jmp = new Blt(cond.firstVar, cond.secondVar, outS); break;
			case "LE": jmp = new Ble(cond.firstVar, cond.secondVar, outS); break;
			case "GT": jmp = new Bgt(cond.firstVar, cond.secondVar, outS); break;
			case "GE": jmp = new Bge(cond.firstVar, cond.secondVar, outS); break;
			case "EQ": jmp = new Beq(cond.firstVar, cond.secondVar, outS); break;
			case "NE": jmp = new Bne(cond.firstVar, cond.secondVar, outS); break;
			case "FEQ":
				regi = generateTemp(Scope.Type.INT);
				jmp = new Feq(cond.firstVar, cond.secondVar, regi);
				co.code.add(jmp);
				jmp = new Bne(regi, "x0", outS);
				break;
			case "FNE":
				regi = generateTemp(Scope.Type.INT);
				jmp = new Feq(cond.firstVar, cond.secondVar, regi);
				co.code.add(jmp);
				jmp = new Beq(regi, "x0", outS);
				break;
			case "FLT": 
				regi = generateTemp(Scope.Type.INT);
				jmp = new Flt(cond.firstVar, cond.secondVar, regi); 
				co.code.add(jmp);
				jmp = new Bne(regi, "x0", outS);
				break;
			case "FLE":
				regi = generateTemp(Scope.Type.INT);
				jmp = new Fle(cond.firstVar, cond.secondVar, regi); 
				co.code.add(jmp);
				jmp = new Bne(regi, "x0", outS);
				break;
			default: throw new Error("Cond have a weird type");
		}
		co.code.add(jmp);
		co.code.addAll(slist.code);
		jmp = new J(loopS);
		co.code.add(jmp);
		co.code.add(outL);

		return co;
	}

	/**
	 * FILL IN FOR STEP 4
	 * 
	 * Generating code for returns
	 * 
	 * Step 0: Generate new code object
	 * 
	 * Step 1: Add retExpr code to code object (rvalify if necessary)
	 * 
	 * Step 2: Store result of retExpr in appropriate place on stack (fp + 8)
	 * 
	 * Step 3: Jump to out label (use @link{generateFunctionOutLabel()})
	 */
	@Override
	protected CodeObject postprocess(ReturnNode node, CodeObject retExpr) {
		CodeObject co = new CodeObject();

		/* FILL IN */
		if(retExpr == null){
			Instruction jmp = new J(generateFunctionOutLabel());
			co.code.add(jmp);
			return co;
		}
		if(retExpr.lval){
			retExpr = rvalify(retExpr);
		}
		co.code.addAll(retExpr.getCode());
		Instruction save;
		if(retExpr.getType() == Scope.Type.FLOAT){
			save = new Fsw(retExpr.temp, "fp", "8");
			
		}
		else{
			save = new Sw(retExpr.temp, "fp", "8");
		}
		Instruction jmp = new J(generateFunctionOutLabel());
		co.code.add(save);
		co.code.add(jmp);
		return co;
	}

	@Override
	protected void preprocess(FunctionNode node) {
		// Generate function label information, used for other labels inside function
		currFunc = node.getFuncName();

		//reset register counts; each function uses new registers!
		intRegCount = 0;
		floatRegCount = 0;
	}

	/**
	 * FILL IN FOR STEP 4
	 * 
	 * Generate code for functions
	 * 
	 * Step 1: add the label for the beginning of the function
	 * 
	 * Step 2: manage frame  pointer
	 * 			a. Save old frame pointer
	 * 			b. Move frame pointer to point to base of activation record (current sp)
	 * 			c. Update stack pointer
	 * 
	 * Step 3: allocate new stack frame (use scope infromation from FunctionNode)
	 * 
	 * Step 4: save registers on stack (Can inspect intRegCount and floatRegCount to know what to save)
	 * 
	 * Step 5: add the code from the function body
	 * 
	 * Step 6: add post-processing code:
	 * 			a. Label for `return` statements inside function body to jump to
	 * 			b. Restore registers
	 * 			c. Deallocate stack frame (set stack pointer to frame pointer)
	 * 			d. Reset fp to old location
	 * 			e. Return from function
	 */
	@Override
	protected CodeObject postprocess(FunctionNode node, CodeObject body) {
		CodeObject co = new CodeObject();

		/* FILL IN */
		//add label
		Label func = new Label(generateFunctionLabel(node.getFuncName()));
		co.code.add(func);
		//save old frame pointer
		Instruction temp = new Sw("fp", "sp", "0");
		co.code.add(temp);
		//make fp = sp
		temp = new Mv("sp", "fp");
		co.code.add(temp);
		//update sp
		temp = new Addi("sp", "-4", "sp");
		co.code.add(temp);
		//allocate new stack frame
		int localNum = node.getScope().getNumLocals();
		localNum = (-4) * localNum;
		temp = new Addi("sp", String.valueOf(localNum), "sp");
		co.code.add(temp);
		//save register to stack
		for (int i = 1;i <= getIntRegCount(); i++) {
			temp = new Sw("t"+String.valueOf(i), "sp", "0");
			co.code.add(temp);
			temp = new Addi("sp", "-4", "sp");
			co.code.add(temp);
		}
		for (int i = 1;i <= getFloatRegCount(); i++) {
			temp = new Fsw("f"+String.valueOf(i), "sp", "0");
			co.code.add(temp);
			temp = new Addi("sp", "-4", "sp");
			co.code.add(temp);
		}
		//add code from body
		co.code.addAll(body.getCode());
		//define return label
		Label rtn = new Label(generateFunctionOutLabel());
		co.code.add(rtn);
		//restore registers
		for(int i = floatRegCount; i > 0; i--){
			temp = new Addi("sp", "4", "sp");
			co.code.add(temp);
			temp = new Flw("f"+String.valueOf(i), "sp", "0");
			co.code.add(temp);
		}
		for(int i = intRegCount; i > 0; i--){
			temp = new Addi("sp", "4", "sp");
			co.code.add(temp);
			temp = new Lw("t"+String.valueOf(i), "sp", "0");
			co.code.add(temp);
		}
		//restore stack
		temp = new Mv("fp", "sp");
		co.code.add(temp);
		temp = new Lw("fp", "fp", "0");
		co.code.add(temp);
		temp = new Ret();
		co.code.add(temp);
		return co;
	}

	/**
	 * Generate code for the list of functions. This is the "top level" code generation function
	 * 
	 * Step 1: Set fp to point to sp
	 * 
	 * Step 2: Insert a JR to main
	 * 
	 * Step 3: Insert a HALT
	 * 
	 * Step 4: Include all the code of the functions
	 */
	@Override
	protected CodeObject postprocess(FunctionListNode node, List<CodeObject> funcs) {
		CodeObject co = new CodeObject();

		co.code.add(new Mv("sp", "fp"));
		co.code.add(new Jr(generateFunctionLabel("main")));
		co.code.add(new Halt());
		co.code.add(new Blank());

		//add code for each of the functions
		for (CodeObject c : funcs) {
			co.code.addAll(c.code);
			co.code.add(new Blank());
		}

		return co;
	}

	/**
	* 
	* FILL IN FOR STEP 4
	* 
	* Generate code for a call expression
	 * 
	 * Step 1: For each argument:
	 * 
	 * 	Step 1a: insert code of argument (don't forget to rvalify!)
	 * 
	 * 	Step 1b: push result of argument onto stack 
	 * 
	 * Step 2: alloate space for return value
	 * 
	 * Step 3: push current return address onto stack
	 * 
	 * Step 4: jump to function
	 * 
	 * Step 5: pop return address back from stack
	 * 
	 * Step 6: pop return value into fresh temporary (destination of call expression)
	 * 
	 * Step 7: remove arguments from stack (move sp)
	 * 
	 * Add special handling for malloc and free
	 */

	 /**
	  * FOR STEP 6: Make sure to handle VOID functions properly
	  */
	@Override
	protected CodeObject postprocess(CallNode node, List<CodeObject> args) {
		
		//STEP 0
		CodeObject co = new CodeObject();
		Instruction temp;
		/* FILL IN */
		//insert code of arguments
		for(CodeObject argN: args){
			if(argN.lval){
				argN = rvalify(argN);
			}
			co.code.addAll(argN.getCode());
			if(argN.getType() == Scope.Type.FLOAT){
				temp = new Fsw(argN.temp, "sp", "0");
			}
			else{
				temp = new Sw(argN.temp, "sp", "0");
			}
			co.code.add(temp);
			temp = new Addi("sp", "-4", "sp");
			co.code.add(temp);
		}
		//space for return value
		temp = new Addi("sp", "-4", "sp");
		co.code.add(temp);
		//push current ra
		temp = new Sw("ra", "sp", "0");
		co.code.add(temp);
		temp = new Addi("sp", "-4", "sp");
		co.code.add(temp);
		temp = new Jr(generateFunctionLabel(node.getFuncName()));
		co.code.add(temp);
		temp = new Addi("sp", "4", "sp");
		co.code.add(temp);
		temp = new Lw("ra", "sp", "0");
		co.code.add(temp);
		// if(node.getFuncName.equals())
		temp = new Addi("sp", "4", "sp");
		co.code.add(temp);
		// System.out.println(node.getFuncName() + "====" + node.getType());
		if(node.getType() == Scope.Type.FLOAT){
			co.temp = generateTemp(node.getType());
			temp = new Flw(co.temp, "sp", "0");
			co.code.add(temp);
		}
		else if(node.getType() != Scope.Type.VOID){
			co.temp = generateTemp(node.getType());
			temp = new Lw(co.temp, "sp", "0");
			co.code.add(temp);
		}
		temp = new Addi("sp", String.valueOf(args.size() * 4), "sp");
		co.code.add(temp);
		return co;
	}	
	
	/**
	 * Generate code for * (expr)
	 * 
	 * Goal: convert the r-val coming from expr (a computed address) into an l-val (an address that can be loaded/stored)
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Rvalify expr if needed
	 * 
	 * Step 2: Copy code from expr (including any rvalification) into new code object
	 * 
	 * Step 3: New code object has same temporary as old code, but now is marked as an l-val
	 * 
	 * Step 4: New code object has an "unwrapped" type: if type of expr is * T, type of temporary is T. Can get this from node
	 */
	@Override
	protected CodeObject postprocess(PtrDerefNode node, CodeObject expr) {
		CodeObject co = new CodeObject();
		/* FILL IN FOR STEP 6 */
		if(expr.lval == true){
			expr = rvalify(expr);
		}
		// System.out.println(expr.type);
		co.code.addAll(expr.code);
		co.temp = expr.temp;
		// System.out.println("******" + co.temp);
		co.lval = true;
		Scope.Type newT = expr.getType().getWrappedType();
		co.type = newT;
		// System.out.println(co.type);
		return co;
	}

	/**
	 * Generate code for a & (expr)
	 * 
	 * Goal: convert the lval coming from expr (an address) to an r-val (a piece of data that can be used)
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: If lval is a variable, generate code to put address into a register (e.g., generateAddressFromVar)
	 *			Otherwise just copy code from other code object
	 * 
	 * Step 2: New code object has same temporary as existing code, but is an r-val
	 * 
	 * Step 3: New code object has a "wrapped" type. If type of expr is T, type of temporary is *T. Can get this from node
	 */
	@Override
	protected CodeObject postprocess(AddrOfNode node, CodeObject expr) {
		CodeObject co = new CodeObject();
		// System.out.println("$$$" + expr.type);
		if(expr.isVar()){
			co.code.addAll(generateAddrFromVariable(expr));
			expr.temp = co.code.getLast().getDest();
		}
		else{
			co.code.addAll(expr.code);
		}
		co.temp = expr.temp;
		co.lval = false;
		Scope.Type newT = Scope.Type.PTR;
		newT.setWrappedType(expr.getType());
		co.type = newT;
		// System.out.println("&&&&&" + co);
		return co;
	}

	/**
	 * Generate code for malloc
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Add code from expression (rvalify if needed)
	 * 
	 * Step 2: Create new MALLOC instruction
	 * 
	 * Step 3: Set code object type to INFER
	 */
	@Override
	protected CodeObject postprocess(MallocNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 6 */
		if(expr.lval == true){
			expr = rvalify(expr);
		}
		co.code.addAll(expr.code);
		String temp = generateTemp(Scope.Type.INT);
		co.code.add(new Malloc(expr.temp, temp));
		co.temp = temp;
		co.type = Scope.Type.INFER;
		return co;
	}
	
	/**
	 * Generate code for free
	 * 
	 * Step 0: Create new code object
	 * 
	 * Step 1: Add code from expression (rvalify if needed)
	 * 
	 * Step 2: Create new FREE instruction
	 */
	@Override
	protected CodeObject postprocess(FreeNode node, CodeObject expr) {
		CodeObject co = new CodeObject();

		/* FILL IN FOR STEP 6 */
		if(expr.lval){
			expr = rvalify(expr);
		}
		co.code.addAll(expr.code);
		co.code.add(new Free(expr.temp));

		return co;
	}

	/**
	 * Generate a fresh temporary
	 * 
	 * @return new temporary register name
	 */
	protected String generateTemp(Scope.Type t) {
		switch(t) {
			case INT: 
			case PTR: //works the same for pointers
				return intTempPrefix + String.valueOf(++intRegCount);
			case FLOAT: return floatTempPrefix + String.valueOf(++floatRegCount);
			default: throw new Error("Generating temp for bad type");
		}
	}

	protected String generateLoopLabel() {
		return "loop_" + String.valueOf(++loopLabel);
	}

	protected String generateElseLabel() {
		return  "else_" + String.valueOf(++elseLabel);
	}

	protected String generateOutLabel() {
		return "out_" +  String.valueOf(++outLabel);
	}

	protected String generateFunctionLabel() {
		return "func_" + currFunc;
	}

	protected String generateFunctionLabel(String func) {
		return "func_" + func;
	}

	protected String generateFunctionOutLabel() {
		return "func_ret_" + currFunc;
	}
	
	/**
	 * Take a code object that results in an lval, and create a new code
	 * object that adds a load to generate the rval.
	 * 
	 * @param lco The code object resulting in an address
	 * @return A code object with all the code of <code>lco</code> followed by a load
	 *         to generate an rval
	 */
	protected CodeObject rvalify(CodeObject lco) {
		
		assert (lco.lval == true);
		//Step 0
		CodeObject co = new CodeObject();
		assert (lco.lval == true);
		//step 1
		//Step 1
  		if (lco.isVar()){
   			co.code.addAll(generateAddrFromVariable(lco));
   			lco.temp = co.code.getLast().getDest();
  		}else{
   			co.code.addAll(lco.getCode());
  		}
  		//Step 2
  		Instruction loadTemp = null;
  		if(lco.getType() == Scope.Type.INT){
			loadTemp = new Lw(generateTemp(lco.getType()), lco.temp, "0");
		}
		else if(lco.getType() == Scope.Type.FLOAT){
			loadTemp = new Flw(generateTemp(lco.getType()), lco.temp, "0");
		}
		else if(lco.getType() == Scope.Type.PTR){
			// if(lco.getType().getWrappedType() == Scope.Type.FLOAT){
			// 	loadTemp = new Flw(generateTemp(lco.getType()), lco.temp, "0");
			// }
			// else{
				loadTemp = new Lw(generateTemp(lco.getType()), lco.temp, "0");
			// }
		}
  		co.code.add(loadTemp);
  		co.lval = false;
  		co.temp = loadTemp.getDest();
  		co.type = lco.getType();

  		return co;
	}

	/**
	 * Generate an instruction sequence that holds the address of the variable in a code object
	 * 
	 * If it's a global variable, just get the address from the symbol table
	 * 
	 * If it's a local variable, compute the address relative to the frame pointer (fp)
	 * 
	 * @param lco The code object holding a variable
	 * @return a list of instructions that puts the address of the variable in a register
	 */
	private InstructionList generateAddrFromVariable(CodeObject lco) {

		InstructionList il = new InstructionList();

		//Step 1:
		SymbolTableEntry symbol = lco.getSTE();
		String address = symbol.addressToString();

		//Step 2:
		Instruction compAddr = null;
		if (symbol.isLocal()) {
			//If local, address is offset
			//need to load fp + offset
			//addi tmp' fp offset
			compAddr = new Addi("fp", address, generateTemp(Scope.Type.INT));
		} else {
			//If global, address in symbol table is the right location
			//la tmp' addr //Register type needs to be an int
			compAddr = new La(generateTemp(Scope.Type.INT), address);
		}
		il.add(compAddr); //add instruction to code object

		return il;
	}

}
