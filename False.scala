import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap

object False {
	def main(args: Array[String]) {
		println("Sort");
		False START "6 9 5 8 7 49 1 7s:1i:[s;0>][1i:[s;i;>][$i;1+ø\\>~[i;`]?i;1+i:]#.\" \"s;1-s:]#" 
		
		println("Prime number from 1 to 100");
		False START "[\\$@$@\\/*-0=]d:  {Test if p divides q}[$2/[\\$@$@\\d;!~][1-]#1=\\%]p:   {Is p prime?}[[$1=~][$p;![$.\" \"]?1-]#]f:  {for all i from n to 2 do { if i is prime then print i} }99f;!";
		
		println("16 Fibonacci");
		False START "0i:1a:1b:[i;16=~][a;$.\", \"$b;$a:+b:i;1+i:]#\"...\""; 
		
		println("Quine");
		False START "[\"'[,34,$!34,'],!\"]'[,34,$!34,'],!";  
		
		println("9!");
		False START "[$1=$[\\%1\\]?~[$1-f;!*]?]f:9f;!."; 
		
		println("reverse input list");
		False START "[][^$1_=~][[.!]]#%!"; 
		
		//False START "ß[^$1_=~][,]#";
		//False START "1a:[a;5\\>][a;1+a:]#a;
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
				//Comment
				case '{' => i = i+getComment(i+1)+1
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
					stack.pop;
					val n = stack.pop;
					val ar:Array[String] = stack.toArray;
					stack.push(ar(n.toInt));
				} 
				//Extension of ø rotates the nth element to the top instead of copies it
				case '`' => {
					var tempL:Stack[String]  = new Stack[String];
					val n = stack.pop.toInt;
					for(i:Int <- 0 until n) {
						tempL.push(stack.pop);
					}
					val res = stack.pop;
					for(i:Int <- 0 until tempL.length) {
						stack.push(tempL.pop);
					}
					stack.push(res);
				}
				case ''' => {
					val char = prog(i+1);
					stack.push(char.toInt.toString);
					i = i+1;
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
				case '.' => {
					try {
						print(stack.pop);
					} catch {
						case e:Exception => println("stack empty");
					}
				}
				case ',' => {
					val isANum = stack.pop;
					try {
						val char = isANum.toInt
						print(char.toChar)
					} catch {
						case e:Exception => print(isANum);
					}
				}
				case '"' => {
					val s:String = getString(i+1);
					print(s);
					i = i+s.length+1;
				}
				case '^' =>  {
					try {
						val char = readChar;
						stack.push(char.toString);
					}
					catch {
						case e:Exception => stack.push("-1")
					}
				}
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
					val loop = cond+"["+func+"["+cond+"]["+func+"]#]?";
					muProg = addToProg(i, muProg, loop);
					prog = muProg.toCharArray;
				}
				case _ => false
			}
			i = i+1;
			//println(stack.toString+" "+stack.length);
		
		}
		println();
		//println(stack.toString+" "+stack.length);
		//println(map.toString);
		
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

	def getComment(start:Int): Int = {
		var length = 0;
		for(i:Int <- start until prog.length) {
			if(prog(i) != '}') {
				length = length + 1;
			} else {
				return length;
			}
		}
		length
	}


	def getFunc(start:Int): String = {
		var ret = new String;
		var bracket = new Stack[Char];
		bracket.push('[');
		for(i:Int <- start until prog.length) {
			if(prog(i) == '[') {
				bracket.push('[');
				ret = ret + prog(i);
			} else if(prog(i) == ']' && bracket.length == 1 && bracket.top == '[') {
				return ret;
			} else if(prog(i) == ']') {
				bracket.pop;
				ret = ret + prog(i);
			} else {
				ret = ret + prog(i);
			}
		}
		ret
	}	
	
}
