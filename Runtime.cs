using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

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
	
	public object getCopy(bool to_var) {
		object copy = null;
		if(value is Record)
			copy = ((Record) value).getCopy();
		else if(value is Array)
			copy = ((Array) value).getCopy();
		else if(value is Cell)
			copy = ((Cell) value).getCopy(to_var);
		else 
			copy = value;
		if (to_var)
			return new Cell(copy);
		else
			return copy;
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
	public object getCopy() { return new Closure(stackFrame, ftn); }
}

public class Record {
	private Dictionary<string, object> record;

	public Record(){
		this.record = new Dictionary<string, object>();
	}
	
	public object GetValue(string key){
		return record[key];
	}
	
	public void SetValue(string key, object val){
		this.record.Remove(key);
		this.record.Add(key, val);
	}

	public object getCopy(){
		Record copy_ = new Record();
		Dictionary<string, object> copy = copy_.record;
		foreach (KeyValuePair<string, object> pair in record){
			string key = pair.Key;
			object val = pair.Value;
			
			if(val is Record){
				object copyR = ((Record) val).getCopy();
				copy.Add(key, new Cell(copyR));
			} else if(val is Array) {
				object copyA = ((Array) val).getCopy();
				copy.Add(key, new Cell(copyA));
			} else if(val is Cell){
				object copyC = ((Cell) val).getCopy(true);
				copy.Add(key, copyC);
			} else 
				copy.Add(key, new Cell(val));
		}
		return copy_;
	}
}

public class Array {
	private object[] array;
	
	public Array(int size){
		this.array = new object[size];
	}
	
	public Array(int size, object _default_){
		this.array = new object[size];
		for (int i = 0; i < size; i++){
			if (_default_ is Array)
				array[i] = ((Array)_default_).getCopy();
			else if ( _default_ is Record)
				array[i] = ((Record)_default_).getCopy();
			else if (_default_ is Cell) 
				array[i] = ((Cell) _default_).getCopy(true);
			else
				array[i] = _default_;
		}
	}
	
	public void Set(int index, object elem){
		array[index] = elem;
	}
	
	public object Get(int index){
		return array[index];
	}
	
	public object getCopy(){
		Array copy = new Array(array.Length, null);
		for (int i = 0; i < array.Length; i++){
			if (array[i] is Array)
				copy.Set(i, new Cell(((Array)array[i]).getCopy()));
			else if ( array[i] is Record)
				copy.Set(i, new Cell(((Record)array[i]).getCopy()));
			else if(array[i] is Cell)
				copy.Set(i, ((Cell) array[i]).getCopy(true));
			else
				copy.Set(i, new Cell(array[i]));
		}
		return copy;
	}
}

public class Reader {
	string[] buffer;
	int curr;
	
	public Reader(){
		curr = 0;
		buffer = new string[0];
	}
	
	private string read(){
		if (curr == buffer.Length){
			Regex r = new Regex(@"\s+");
			buffer = r.Split(Console.ReadLine());
			curr = 0;
			return read();
		} else {
			string token = buffer[curr];
			curr++;
			return token;
		}
	}
	
	public int readInt(){
		string token = read();
		return Convert.ToInt32(token);
	}
	
	public bool readBool(){
		string token = read();
		return Convert.ToBoolean(token);
	}
	
	public string readString(){
		return read();
	}
	
	public void readLine(){
		buffer = new string[0];
		curr = 0;
	}

}
