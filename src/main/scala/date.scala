class MyDate (val day : Int, val month : Int, val year : Int){
  override def toString() : String =
    "MyDate(" + day.toString + ", " + month.toString + ", " + year.toString + ")"

  override def equals(d : Any) : Boolean =
    d match {
      case d1 : MyDate => (day == d1.day) && (month == d1.month) && (year == d1.year)
      case _ => false
    }

  def daysInMonth() : Int =
    month match {
      case 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
      case 2 => if(Utils.isLeapYear(year)) { 29 } else { 28 }
      case 4 | 6 | 9 | 11 => 30
    }

  def nextDate() : MyDate =
    if(day == daysInMonth())
      if(month == 12) 
        new MyDate(1, 1, year + 1)
      else
        new MyDate(1, month + 1, year)
    else
      new MyDate(day + 1, month, year)

  def isLater(d : MyDate) : Boolean =
    if(year > d.year) true
    else if(year < d.year) false
    else if(month > d.month) true
    else if(month < d.month) false
    else day > d.day

  /* This method computes the days between the current date
   * and the date d.
   */
  def daysInBetween(d : MyDate) : Int =
    if(d.isLater(this)) {
      var count = 0
      var d1 = new MyDate(day, month, year)
      while(!d1.equals(d)) {
        count += 1
        d1 = d1.nextDate
      }
      count
    }
    else
      -d.daysInBetween(this)

  def weekday() : Weekday = {
    val refdate = new MyDate(7, 7, 1975)
    val refWeekday = Weekday.MON

    val n = refdate.daysInBetween(this)
    var wddiff = n % 7
    if(wddiff < 0) {
      wddiff += 7
    }
    val nRefweekday = Weekday.intOfWeekday(refWeekday)
    var nWeekday = nRefweekday + wddiff
    if(nWeekday > 7) {
      nWeekday -= 7
    }
    Weekday.weekdayOfInt(nWeekday)
  }
}

sealed abstract class Weekday(val label: String)
object Weekday {
  final case object SUN extends Weekday(label = "Sunday")
  final case object MON extends Weekday(label = "Monday")
  final case object TUE extends Weekday(label = "Tuesday")
  final case object WED extends Weekday(label = "Wednesday")
  final case object THU extends Weekday(label = "Thursday")
  final case object FRI extends Weekday(label = "Friday")
  final case object SAT extends Weekday(label = "Saturday")

  def nextWeekday(w : Weekday) : Weekday = {
    w match {
      case SUN => MON
      case MON => TUE
      case TUE => WED
      case WED => THU
      case THU => FRI
      case FRI => SAT
      case SAT => SUN
    }
  }

  def intOfWeekday(w : Weekday) : Int = {
    w match {
      case SUN => 1
      case MON => 2
      case TUE => 3
      case WED => 4
      case THU => 5
      case FRI => 6
      case SAT => 7
    }
  }

  def weekdayOfInt(n : Int) : Weekday = {
    n match {
      case 1 => SUN
      case 2 => MON
      case 3 => TUE
      case 4 => WED
      case 5 => THU
      case 6 => FRI
      case 7 => SAT
      case _ => throw new Exception("Invalid weekday number " + n)
    }
  }
}

object Utils {
  def isDivisibleBy(x : Int, y : Int) = x % y == 0

  def isLeapYear(y : Int) =
    if(isDivisibleBy(y, 100))
      isDivisibleBy(y, 400)
    else
      isDivisibleBy(y, 4)

  def monthName(month : Int) =
    month match {
      case 1  => "JAN"
      case 2  => "FEB"
      case 3  => "MAR"
      case 4  => "APR"
      case 5  => "MAY"
      case 6  => "JUN"
      case 7  => "JUL"
      case 8  => "AUG"
      case 9  => "SEP"
      case 10 => "OCT"
      case 11 => "NOV"
      case 12 => "DEC"
      case _ => throw new Exception("Invalid month number " + month)
    }
}
/*
object DateDriver {
  def main (args:Array[String]) {
    println("Hello world")
    val d1 = new MyDate(22, 4, 2025)
    println(d1)
    println(Utils.isDivisibleBy(19, 3))
    println(Weekday.intOfWeekday(Weekday.SUN))
    println(Weekday.weekdayOfInt(1))
    println(Weekday.nextWeekday(Weekday.SUN))
    println(Weekday.nextWeekday(Weekday.SAT))
    println("Weekday(03-05-2025) = " + new MyDate(3, 5, 2025).weekday)
    println("Weekday(24-01-2009) = " + new MyDate(24, 1, 2009).weekday)
  }
}
*/
