package models
import PatientType._
import scala.collection._

object AnalysisResult {
	
	val drugDurations 	= Map("I" -> 90,			  "B" -> 30)
	
	// switching to drug {key}
	val switches 		= Map("I" -> VALID_BI_SWITCH, "B" -> VALID_IB_SWITCH)
	
	// trialing drug {key}
	val trials   		= Map("I" -> VALID_I_TRIAL,   "B" -> VALID_B_TRIAL)
  
	def performBIAnalysis(): Map[PatientType, Int] =
	{

      val results = mutable.Map.empty[PatientType, Int]
	  PatientType.values foreach {
		  results(_) = 0
	  }
      
      // for each customer, pull out their worst status and record that to the total
      testClassify.values foreach {customer =>
      	val x = ((customer unzip) _1) max;
      	results(x) += 1;
      }      
      results
	}
	
	
	def dumpData(): Map[Int, List[(PurchaseRecord, Int)]] =
	{
		val x = PurchaseRecord
				.loadFromFile
				.sortBy(_.day)
				.groupBy(_.patientID)
				
		x.mapValues {t => 
		 	t zip 0 :: (t sliding(2) map {case List(p,q) => q.day - p.day; case List(p) => 0} toList)			
		}		
	}
	
	
	def testClassify(): Map[Int, List[(PatientType,PurchaseRecord)]] = {	  
	  // load all purchases, sort them by date and break them down by customer
	  val purchases = PurchaseRecord
			  			.loadFromFile
			  			.sortBy(_.day)
			  			.groupBy(_.patientID);
	  
	  val x: Map[Int, List[(PatientType,PurchaseRecord)]] = purchases.mapValues {records =>
		  val (firstRun, secondRun) = records.span(_.medication == records.head.medication)
		  if (firstRun(0).patientID == 82)
		    true
		  
		  // If second run is empty, then patient has only ever been on a single drug
		  if (secondRun == Nil) 
		    List((VALID_NO_COMED, firstRun.last))
		  else 
		    classifyBatch(firstRun, secondRun, VALID_NO_COMED)
		    
	  }
	  x
	}

	
	
	def classifyBatch(previous: List[PurchaseRecord], current: List[PurchaseRecord], status: PatientType): List[(PatientType, PurchaseRecord)] =
	{
	   var newStatus = status
	   
	   // Each segment is constructed to contain only a single drug so if we are here
	   // there has been a switch in medication. Only need to determine whether it is valid
	   // and if it is, for what reason.
	   
	   
	   // if there's no overlap, they are independent admininstrations
	   if (overlaps(previous.last, current.head)) {
	     // 
		 if (current.length == 1)
		   newStatus = if (status == VALID_NO_COMED) trials(current.head.medication) else VIOLATED
		 else
		   newStatus = if (status == VALID_NO_COMED) switches(current.head.medication) else VIOLATED
	   }
	   else {
	     newStatus = VALID_NO_COMED
	   }
	  
	   val retVal = List((newStatus, current.last))
	   
	   // Base and recursive cases for recursion
	   val (newPrevious, newCurrent) = current.span(_.medication == current.head.medication)
	   newCurrent match{
	     case Nil => retVal
	     case _   => retVal ++ classifyBatch(newPrevious, newCurrent, newStatus)
	   }
	}
	
	
	// Return whether purchase of p2 would overlap with p1
	def overlaps(p1: PurchaseRecord, p2: PurchaseRecord ): Boolean  = {
		val timeDifference = p2.day - p1.day;
		timeDifference < drugDurations(p1.medication)
	}

}



