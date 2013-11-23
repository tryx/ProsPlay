package models
import PatientType._
import scala.collection._

object AnalysisResult {

	def performBIAnalysis(): Map[PatientType, Int] =
	{
	  // load all purchases, sort them by date and break them down by customer
	  val purchases = PurchaseRecord
			  			.loadFromFile
			  			.view  					// don't explicitly construct collection 
			  			.sortBy(_.day)
			  			.groupBy(_.patientID)
	  
	  val results = mutable.Map.empty[PatientType, Int]
	  
	  // Initialise the return array 
	  PatientType.values foreach {
	    results(_) = 0
	  }
	  
	  // loop over each customer
	  purchases.values foreach {customer =>
		  
		  println
	  }
	  
	  Map(
	      VIOLATED -> 200,
	      VALID_NO_COMED -> 500,
	      VALID_BI_SWITCH -> 100,
	      VALID_IB_SWITCH -> 100,
	      VALID_I_TRIAL -> 50,
	      VALID_B_TRIAL -> 50
	  )
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
}

