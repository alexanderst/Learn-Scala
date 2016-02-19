
def sum(xs:List[Int]):Int = xs.foldLeft(0)(_ + _)

def last[T](xs:List[T]):T = {
	xs.foldLeft[T](xs.head)((_,c) => c)
}

def beforeLast[T](xs:List[T]):T = {
	xs.foldLeft((list.head, list.tail.head))((r,c) =>
				(r._2, c) )._1
}


def length(xs:List[Int]) : List[Int] = {
	xs match {
		case Nil => 0
		case _ => 1 + length(xs.tail)
	}
}

def lengthByFold(xs:List[Int]):Int = {
	xs.foldLeft(0)((sz,_) => sz + 1)
}

def traverse(xs:List[Int], f:Int => Int) : Unit = {
	var ret = List[Int]()
	var currList = xs
	while (!currList.isEmpty) {
		f(currList.head)
		currList = currList.tail
	}
}

def reverse(xs:List[Int]) : List[Int] = {
	var ret:List[Int] = Nil
	
	def reverseImpl(xs:List[Int], accum:List[Int]):List[Int] = {
		xs match {
			case xs if xs.isEmpty => ret
			case _ => reverseImpl(xs.tail, ret).head :: ret
		}
	}
	ret
}

def reverse1(xs:List[Int]):List[Int] = {
	xs.foldLeft(List[Int]())((a,b) => b :: a)
}


def reverse2(xs:List[Int]) : List[Int] = {
	var ret = List[Int]()
	var currList = xs
	while (!currList.isEmpty) {
		ret = currList.head :: ret
		currList = currList.tail
	}
}

def isPalindrome(xs:List[Int]): Boolean = {
	var fromLeft = xs.foldLeft(List[Int]())((a,b) => b :: a)
	var fromRight = xs.foldRight(List[Int]())((a,b) => a :: b)
	fromLeft == fromRight
}

