import scala.collection.mutable.Stack

object False {
	def main(args: Array[String]) {
		False START "12_ 4 =~0|";
	}

	//The main prog
	var prog = Array.ofDim[Char](0);
	//The stack
	val stack = new Stack[Int];
	
	def START(mainProg: String) = {
		prog = mainProg.toCharArray;
		var i = 0;
		while(i < prog.length) {
			val cur = prog(i);
			if(cur.isDigit) {
				val strNum:String = getInt(i);
				stack.push(strNum.toInt);
				i = i + strNum.length-1;
			}
			cur match {
				//Arithmetic
				case '_' => stack.push(-1 * stack.pop)
				case '+' => stack.push(stack.pop + stack.pop)
				case '-' => {
					swap
					stack.push(stack.pop - stack.pop)
				}
				case '*' => stack.push(stack.pop * stack.pop)
				case '/' => {
					swap
					stack.push(stack.pop / stack.pop)
				}
				//conditional
				case '=' => {
					if(stack.pop == stack.pop) 
						stack.push(-1);
					else 
						stack.push(0);
				}
				case '~' => {
					if(stack.pop == 0) 
						stack.push(-1);
					else 
						stack.push(0);
				}
				case '>' => {
					val a = stack.pop;
					val b = stack.pop;
					if(b > a) 
						stack.push(-1);
					else 
						stack.push(0);
				}
				case '&' => {
					val a = stack.pop;
					val b = stack.pop;
					if(a == 0 || b == 0) 
						stack.push(0);
					else 
						stack.push(-1);
				}
				case '|' => {
					val a = stack.pop;
					val b = stack.pop;
					if(a == -1 || b == -1) 
						stack.push(-1);
					else 
						stack.push(0);
				}
				//stack operations
				case '$' => stack.push(stack.top)
				case '%' => stack.pop
				case '\\' => swap
				case '@' => {
					val first = stack.pop;
					val second = stack.pop;
					val third = stack.pop;
					stack.push(second);
					stack.push(first);
					stack.push(third);
				}
				case 'ø' => {
					val n = stack.pop;
					val ar:Array[Int] = stack.toArray;
					stack.push(ar(n));
				}
				case _ => false
			}
			i = i+1;
		}
		println(stack.toString);
	}

	def swap() = {
		val top:Int = stack.pop;
		val bot:Int = stack.pop;
		stack.push(top);
		stack.push(bot);
	}

	def getInt(start:Int): String = {
		var ret = new String;
		for(i:Int <- start until prog.length) {
			if(prog(i).isDigit) {
				ret = ret + prog(i);
			} else {
				return ret;
			}
		}
		ret
	}

	
	
}
