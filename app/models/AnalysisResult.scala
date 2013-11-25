package models
import PatientType._
import scala.collection._

object AnalysisResult {
	val BDuration = 30
	val IDuration = 90
  
	def performBIAnalysis(): Map[PatientType, Int] =
	{
	  
	  // Initialise the return array 
	  val results = mutable.Map.empty[PatientType, Int]
	  PatientType.values foreach {
		  results(_) = 0
	  }
	  
	  val patientResults = mutable.Map.empty[Int,PatientType]
	  
	  // load all purchases, sort them by date and break them down by customer
	  val purchases = PurchaseRecord
			  			.loadFromFile
			  			.sortBy(_.day)
			  			.groupBy(_.patientID);
	  
	  purchases.values foreach {purchaseList =>
	  	var status = VALID_NO_COMED
	  	val window = purchaseList.sliding(3)

	  	window foreach {
	  	case List(p1, p2, p3) => {
		  val timeDifference = p2.day - p1.day
		  val timeDifference2 = p3.day - p2.day

		  // if there's no change in medication, can't have a new violation
		  if (p1.medication != p2.medication) {
		  p2.medication match {

			  case "B" => {
				if(timeDifference < BDuration) status match {
				  // If patient was in violation, this can't help things	 		  	
				  case VIOLATED => VIOLATED		
		
				  // assume first violation is a trial
				  case VALID_NO_COMED => 
				  if (p1.medication == p3.medication) VALID_B_TRIAL else VALID_IB_SWITCH
		
				  case VALID_IB_SWITCH => VIOLATED
				  case VALID_BI_SWITCH => VIOLATED
				  case VALID_B_TRIAL => VALID_B_TRIAL
				  case VALID_I_TRIAL => VALID_NO_COMED
				}
			  }
					  
			  case "I" => {
			    if(timeDifference < BDuration) status match {
				  // If patient was in violation, this can't help things	 		  	
				  case VIOLATED => VIOLATED		
	
				  // assume first violation is a trial
				  case VALID_NO_COMED => 
				    if (p1.medication == p3.medication) VALID_B_TRIAL else VALID_IB_SWITCH
	
				  case VALID_IB_SWITCH => VIOLATED
				  case VALID_BI_SWITCH => VIOLATED
				  case VALID_B_TRIAL => VALID_B_TRIAL
				  case VALID_I_TRIAL => VALID_NO_COMED
				  }
				  
			  }
		  }}
	  	}
	  	case List(p1,p2) => status;
	  	case List(p1) 	 => status
	  	
	  	val patient = purchaseList(0).patientID
	  	patientResults(patient) = status;
	  	}
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
	
	def testClassify(): Map[Int, List[(PatientType,PurchaseRecord)]] = {
	  // Initialise the return array 
	  val results = mutable.Map.empty[PatientType, Int]
	  PatientType.values foreach {
		  results(_) = 0
	  }
	  
	  val patientResults = mutable.Map.empty[Int,PatientType]
	  
	  // load all purchases, sort them by date and break them down by customer
	  val purchases = PurchaseRecord
			  			.loadFromFile
			  			.sortBy(_.day)
			  			.groupBy(_.patientID);
	  
	  purchases.mapValues {purchaseList =>
	    val window = purchaseList.sliding(3)
	    
	    val statuses: List[PatientType] = window.scanLeft (VALID_NO_COMED)
	    	{ (status, window) => parseWindow(window, status)} toList;
	    val (sl, pl) = (statuses.length, purchaseList.length)
	    (VALID_NO_COMED :: statuses) zip purchaseList 
	  }
	}
	
	
	// Return whether purchase of p2 would overlap with p1
	def overlaps(p1: PurchaseRecord, p2: PurchaseRecord ): Boolean  = {
		val timeDifference = p2.day - p1.day;
		p2.medication match {
		  case "B" => timeDifference > BDuration
		  case "I" => timeDifference > IDuration
		}
	}
	
	
	def parseWindow(window: List[models.PurchaseRecord], status: PatientType)
		:(PatientType) =
	{
	 window match {
  	 case List(p1, p2, p3) => {
		// if there's no change in medication, can't have a new violation
		if (p1.medication != p2.medication) {
		p2.medication match {

		  case "B" => {
			if(overlaps(p1, p2)) status match {
			  // If patient was in violation, this can't help things	 		  	
			  case VIOLATED => VIOLATED		
		
			  // assume first violation is a trial
			  case VALID_NO_COMED => 
			  if (p1.medication == p3.medication) VALID_I_TRIAL else VALID_BI_SWITCH
		
			  case VALID_IB_SWITCH => VIOLATED
			  case VALID_BI_SWITCH => VIOLATED
			  case VALID_B_TRIAL => VALID_B_TRIAL
			  case VALID_I_TRIAL => VALID_NO_COMED
			} else status
		  }
					  
		  case "I" => {
		    if(overlaps(p1, p2)) status match {
			  // If patient was in violation, this can't help things	 		  	
			  case VIOLATED => VIOLATED		

			  // assume first violation is a trial
			  case VALID_NO_COMED => 
			    if (p1.medication == p3.medication) VALID_B_TRIAL else VALID_IB_SWITCH

			  case VALID_IB_SWITCH => VIOLATED
			  case VALID_BI_SWITCH => VIOLATED
			  case VALID_B_TRIAL => VALID_B_TRIAL
			  case VALID_I_TRIAL => VALID_NO_COMED
			} else status
				  
		  }
		}} else status
	 }
	 case List(p1,p2) => status
	 case List(p1) 	 => status
	 }
    }

}



