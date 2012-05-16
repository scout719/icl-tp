using System;

class Cell {
	object value;
	
	public Cell(object v) {
		value = v; 
	}
	
	public object get() { 
		return value; 
	}
	
	public void set(object v) { 
		value = v;
	}
}

class StackFrame {
	object staticlink; // indice 0

	object[] paramS;
	// 1 a nParams
	int nParams;
	
	object[] locals;
	// ... > nParams+1
	
	public StackFrame(object SL) {
		this.staticlink = SL;
	}
	
	public void initArgs(int nParams) {
		this.nParams = nParams;
		paramS = new object[nParams];
	}
	
	public void initLocals(int nLocals){
		locals = new object[nLocals];
	}
	
	public object get(int i){
		if(i == 0){
			return staticlink;
		} else if(i > nParams ){
			return locals[i - nParams - 1];
		} else {
			return paramS[i - 1];
		}
	}
	
	public void set(int i, object o){
		if(i == 0)
			staticlink = o;
		else if(i > nParams )
			locals[i - nParams - 1] = o;
		else
			paramS[i - 1] = o;
	}
}

class Closure {
	StackFrame stackFrame;
	IntPtr ftn;

	public Closure(StackFrame stackFrame, IntPtr ftn) {
		this.stackFrame = stackFrame;
		this.ftn = ftn;
	}

	public StackFrame getSF() { return stackFrame; }
	public IntPtr getFtn() { return ftn; }
}
