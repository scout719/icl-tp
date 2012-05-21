using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

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
	
	public void InitArgs(int nParams) {
		this.nParams = nParams;
		paramS = new object[nParams];
	}
	
	public void InitLocals(int nLocals){
		locals = new object[nLocals];
	}
	
	public object Get(int i){
		if(i == 0){
			return staticlink;
		} else if(i > nParams ){
			return locals[i - nParams - 1];
		} else {
			return paramS[i - 1];
		}
	}
	
	public void Set(int i, object o){
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

	public StackFrame GetSF() {
		return stackFrame;
	}
	
	public IntPtr GetFtn() {
		return ftn;
	}
	
	public void SetClosure(Closure new_closure){
		this.stackFrame = new_closure.GetSF();
		this.ftn = new_closure.GetFtn();
	}
	
	public void CopyTo(object other){
		Closure destination = (Closure) other;
		destination.SetClosure(new Closure(this.stackFrame, this.ftn));
	}
	
}

class Cell {
	object value;
	
	public Cell(object v) {
		value = v; 
	}
	
	public object Get() {
		return value; 
	}
	
	public void Set(object v) {
			value = v;
	}
	
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

	public void CopyTo(object other){
		Record destination = (Record) other;
		foreach (KeyValuePair<string, object> pair in record){
			string key = pair.Key;
			object val = pair.Value;
			
			if(val is Record){
				Record rec = (Record) val;
				rec.CopyTo(destination.GetValue(key));
			} else if(val is Array) {
				Array arr = (Array) val;
				arr.CopyTo(destination.GetValue(key));
			} else if(val is Closure) {
				Closure clo = (Closure) val;
				clo.CopyTo(destination.GetValue(key));
			} else if(val is Cell){
				Cell cell = (Cell) val;
				object val2 = cell.Get();
				destination.SetValue(key, new Cell(val2));
			} else 
				destination.SetValue(key, new Cell(val));		
		}
	}
	
	public object GetConstCopy(){
		Record copy = new Record();
		foreach (KeyValuePair<string, object> pair in record){
			string key = pair.Key;
			object val = pair.Value;
			
			if(val is Record){
				Record rec = (Record) val;
				copy.SetValue(key, rec.GetConstCopy());
			} else if(val is Array) {
				Array arr = (Array) val;
				copy.SetValue(key, arr.GetConstCopy());
			} else if(val is Cell){
				Cell cell = (Cell) val;
				object val2 = cell.Get();
				copy.SetValue(key, val2);
			} else 
				copy.SetValue(key, val);		
		}
		return copy;
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
			if (_default_ is Array){
				Array arr = (Array) _default_;
				array[i] = new Array(arr.Size());
				arr.CopyTo(array[i]);
			} else if ( _default_ is Record) {
				Record rec = (Record) _default_;
				array[i] = new Record();
				rec.CopyTo(array[i]);
			} else if ( _default_ is Closure) {
				Closure clo = (Closure) _default_;
				array[i] = new Closure(null, new IntPtr(0));
				clo.CopyTo(array[i]);
			} else if(_default_ is Cell) {
				Cell cell = (Cell) _default_;
				object val = cell.Get();
				array[i] = new Cell(val);
			} else
				array[i] = new Cell(_default_);
		}
	}
	
	public int Size(){
		return array.Length;
	}
	
	public void Set(int index, object elem){
		array[index] = elem;
	}
	
	public object Get(int index){
		return array[index];
	}

	public void CopyTo(object other){
		Array destination = (Array) other;
		for(int i = 0; i < array.Length; i++){
			if (array[i] is Array){
				Array arr = (Array) array[i];
				arr.CopyTo(destination.Get(i));
			} else if ( array[i] is Record) {
				Record rec = (Record) array[i];
				rec.CopyTo(destination.Get(i));
			} else if ( array[i] is Closure) {
				Closure clo = (Closure) array[i];
				clo.CopyTo(destination.Get(i));
			} else if(array[i] is Cell) {
				Cell cell = (Cell) array[i];
				object val = cell.Get();
				destination.Set(i, new Cell(val));
			} else
				destination.Set(i, new Cell(array[i]));
		}
	}

	public object GetConstCopy(){
		Array copy = new Array(this.Size());
		for(int i = 0; i < array.Length; i++){
			if (array[i] is Array){
				Array arr = (Array) array[i];
				copy.Set(i, arr.GetConstCopy());
			} else if ( array[i] is Record) {
				Record rec = (Record) array[i];
				copy.Set(i, rec.GetConstCopy());
			} else if(array[i] is Cell) {
				Cell cell = (Cell) array[i];
				object val = cell.Get();
				copy.Set(i, val);
			} else
				copy.Set(i, array[i]);
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
	
	private string Read(){
		if (curr == buffer.Length){
			Regex r = new Regex(@"\s+");
			buffer = r.Split(Console.ReadLine());
			curr = 0;
			return Read();
		} else {
			string token = buffer[curr];
			curr++;
			return token;
		}
	}
	
	public int ReadInt(){
		string token = Read();
		return Convert.ToInt32(token);
	}
	
	public bool ReadBool(){
		string token = Read();
		return Convert.ToBoolean(token);
	}
	
	public string ReadString(){
		return Read();
	}
	
	public void ReadLine(){
		buffer = new string[0];
		curr = 0;
	}
}
