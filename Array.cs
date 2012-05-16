public class Array {
	private object[] array;
	
	public Array(int size, object _default_){
		array = new object[size];
		int i;
		for (i = 0; i < size; i++){
			array[i] = _default_;
		}
	}
	
	public void Set(int index, object elem){
		array[index] = elem;
	}
	
	public object Get(int index){
		return array[index];
	}
}
