using System;
using System.Collections.Generic;

public class Record {
	private Dictionary<string, object> record;

	public Record(){
		this.record = new Dictionary<string, object>();
	}
	
	public object GetValue(string key){
		return record[key];
	}
	
	public void SetValue(string key, object val){
		record.Add(key, val);
	}

}
