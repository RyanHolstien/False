import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap

object False {
	def main(args: Array[String]) {
		False START "[$1=$[\\%1\\]?~[$1-f;!*]?]f:6f;!";

	}

	//The main prog
	var prog = Array.ofDim[Char](0);
	//The stack
	val stack = new Stack[String];
	//Map of the variables
	var map = new HashMap[String, String];
	
	def START(mainProg: String) = {
		var muProg = mainProg;
		prog = muProg.toCharArray;
		var i = 0;
		while(i < prog.length) {
			val cur = prog(i);
			if(cur.isDigit) {
				val strNum:String = getInt(i);
				stack.push(strNum);
				i = i + strNum.length-1;
			} else if(cur.isLetter) {
				stack.push(""+cur);
			}
			cur match {
				//Arithmetic
				case '_' => stack.push((-1 * stack.pop.toInt).toString)
				case '+' => stack.push((stack.pop.toInt + stack.pop.toInt).toString)
				case '-' => {
					swap
					stack.push((stack.pop.toInt - stack.pop.toInt).toString)
				}
				case '*' => stack.push((stack.pop.toInt * stack.pop.toInt).toString)
				case '/' => {
					swap
					stack.push((stack.pop.toInt / stack.pop.toInt).toString)
				}
				//conditional
				case '=' => {
					if(stack.pop == stack.pop) 
						stack.push("-1");
					else 
						stack.push("0");
				}
				case '~' => {
					if(stack.pop == "0") 
						stack.push("-1");
					else 
						stack.push("0");
				}
				case '>' => {
					val a = stack.pop.toInt;
					val b = stack.pop.toInt;
					if(b > a) 
						stack.push("-1");
					else 
						stack.push("0");
				}
				case '&' => {
					val a = stack.pop.toInt;
					val b = stack.pop.toInt;
					if(a == 0 || b == 0) 
						stack.push("0");
					else 
						stack.push("-1");
				}
				case '|' => {
					val a = stack.pop.toInt;
					val b = stack.pop.toInt;
					if(a == -1 || b == -1) 
						stack.push("-1");
					else 
						stack.push("0");
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
					val ar:Array[String] = stack.toArray;
					stack.push(ar(n.toInt));
				}
				//variable assignment
				case ':' => {
					val key = stack.pop;
					val value = stack.pop;
					map.put(key, value);
				}
				case ';' => {
					val key = stack.pop;
					stack.push(map.get(key).getOrElse("ERROR"));
				}
				//IO
				case '.' => print(stack.pop);
				case ',' => {
					val num = stack.pop.toInt;
					print(num.toChar);
				}
				case '"' => {
					val s:String = getString(i+1);
					print(s);
					i = i+s.length+1;
				}
				case '^' => stack.push(readChar.toString)
				case 'ß' => {
					Console.flush;
					stack.pop();
				}
				//lambda
				case '[' => {
					val func:String = getFunc(i+1);
					stack.push(func);
					i = i+func.length+1;
				}
				case '!' => {
					val func = stack.pop;
					muProg = addToProg(i, muProg, func);
					prog = muProg.toCharArray;
				}
				//control flow
				case '?' => {
					val func = stack.pop;
					val cond = stack.pop;
					if(cond == "-1") {
						muProg = addToProg(i, muProg, func);
						prog = muProg.toCharArray;
					}
				}
				case '#' => {
					val func = stack.pop;
					val cond = stack.pop;
					val loop = cond+"["+func+"]?["+cond+"]["+func+"]#";
					muProg = addToProg(i, muProg, func);
					prog = muProg.toCharArray;
				}
				case _ => false
			}
			i = i+1;
		}
		println(stack.toString+" "+stack.length);
		println(map.toString);
	}

	def addToProg(i:Int, prog:String, func:String):String = {
		val beg = prog.substring(0, i+1);
		var end = "";
		if(i+1 != prog.length) {
			end = prog.substring(i+1, prog.length);
		}
		beg+func+end;
	}

	def swap() = {
		val top:String = stack.pop;
		val bot:String = stack.pop;
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

	def getString(start:Int): String = {
		var ret = new String;
		for(i:Int <- start until prog.length) {
			if(prog(i) != '"') {
				ret = ret + prog(i);
			} else {
				return ret;
			}
		}
		ret
	}

	def getFunc(start:Int): String = {
		var ret = new String;
		for(i:Int <- start until prog.length) {
			if(prog(i) != ']') {
				ret = ret + prog(i);
			} else {
				return ret;
			}
		}
		ret
	}	
	
}
