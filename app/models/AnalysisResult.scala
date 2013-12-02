package models
import PatientType._
import scala.collection._

object AnalysisResult {
	type PatientID = Int
  
	// these are expensive to calculate and we don't want to redo it
	// for every ajax call, so cache them here
	val purchases = PurchaseRecord
					.loadFromFile
					.sortBy(_.day)
					.groupBy(_.patientID)
		
	var resultsCache: Map[PatientType, Int] = null
	var patientsByTypeCache: Map[PatientType, List[List[PurchaseRecord]]] = null
	
	// abstract over the drug types as best we can
	val drugDurations 	= Map("I" -> 90,			  "B" -> 30)
	
	// switching to drug {key}
	val switches 		= Map("I" -> VALID_BI_SWITCH, "B" -> VALID_IB_SWITCH)
	
	// trialing drug {key}
	val trials   		= Map("I" -> VALID_I_TRIAL,   "B" -> VALID_B_TRIAL)
	
	/**
	 * 	Runs the classifier over the dataset returning a tuple consisting of a count of each type
	 *  of patient and the transactions of each patient broken down by their category. 
	 */
	def performBIAnalysis(): (Map[PatientType, Int], Map[PatientType, List[List[PurchaseRecord]]] ) =
	{
	  if (resultsCache != null) {return (resultsCache, patientsByTypeCache)}

	  // need to use immutable maps here as mutable.SortedMap is not yet implemented =\
      var results = immutable.SortedMap.empty[PatientType, Int]
      var patientsByType = immutable.SortedMap.empty[PatientType, List[List[PurchaseRecord]]]

	  PatientType.values foreach {t =>
		  results = results updated (t, 0)
		  patientsByType  = patientsByType updated (t, List.empty[List[PurchaseRecord]])
	  }
           
      // for each customer, pull out their worst status and record that to the total
      // and put that customer into the map for that status type
      classify(purchases).values foreach {customer =>
        val (pTypes, pRecords) = (customer unzip)
      	val pType = pTypes max;
      	results = results updated (pType, results(pType) + 1);
      	patientsByType = patientsByType updated (pType, patientsByType(pType) :+ pRecords)
      }
      if (resultsCache == null) {
        resultsCache = results
        patientsByTypeCache = patientsByType
      }
      (results, patientsByType)
	}
	
	/**
	 * Returns a map from a patient ID to a list of that patient's purchases coupled
	 * with the time since the last purchase. A sliding window is moved along the purchase
	 * history and the difference in dates is computed between each pair.
	 */	
	def dumpData(): Map[PatientID, List[(PurchaseRecord, Int)]] =
	{	
		// must prepend 0 to account for the very first puchase which is unpaired
		purchases.mapValues {t => 
		 	t zip 0 :: (t sliding(2) map {
		 	  case List(p,q) => q.day - p.day
		 	  case List(p) => 0} 
		 	toList)			
		}		
	}
	
	/**
	 * Classifies the whole dataset  
	 */
	def classify(purchases: Map[PatientID,List[models.PurchaseRecord]])
	:Map[PatientID, List[(PatientType,PurchaseRecord)]] = {	  
	  purchases.mapValues {records =>
		  val (firstRun, secondRun) = records.span(_.medication == records.head.medication)
		  
		  // If second run is empty, then patient has only ever been on a single drug
		  if (secondRun == Nil) 
		    List((VALID_NO_COMED, firstRun.last))
		  else 
		    classifyBatch(firstRun, secondRun, VALID_NO_COMED)
		    
		    
	  }
	}

	
	/**
	 * Classifies a run of purchases by examining the previous, current and future runs. Internally, a 
	 * state machine is used to impose a temporal ordering on the purchases.
	 */
	def classifyBatch(previous: List[PurchaseRecord], 
					  current: List[PurchaseRecord], 
					  status: PatientType) 
	:List[(PatientType, PurchaseRecord)] =
	{
	   // By construction, the previous block contains only purchases of a single type of medication
	   // so there must be a change of medications between the last purchase in previous and the first
	   // purchase in current. Note that current DOES NOT have to contain only a single medication, it contains
	   // the entire future purchase history.
	   
	   // selectCurrent is the next block of purchases. We construct it early as occasionally
	   // we need to look into the future to make a decision
	   val (nextMedication, future) = current.span(_.medication == current.head.medication)
	   
	   // if there's no overlap, they are independent admininstrations
	   val newStatus = if (overlaps(previous.last, current.head)) {

	     status match { 
	    	 // if the upcoming block after this one has one element, we'll consider a trial
	         // otherwise, treat it as a switch
		     case VALID_NO_COMED => 
		     {
		       if (nextMedication.length == 1)
		         trials(current.head.medication) 
		       else
		         switches(current.head.medication)
		     }
 			 
		     // if the previous state was a valid trial, then by definition we must now be back
		     // in a no co-medication state
		     case (VALID_I_TRIAL | VALID_B_TRIAL) => VALID_NO_COMED
		     
		     // if we made a switch and have not yet had a chance to go back to non-comedicated
		     // we are now in violation.
		     case (VALID_IB_SWITCH | VALID_BI_SWITCH) => VIOLATED
		     
		     // if we were violating before, another switch can in no way turn it into a valid state
		     case VIOLATED => VIOLATED
	     }
	   }
	   // if there is no overlap, we can reset the state and treat the rest independently
	   else {
	     VALID_NO_COMED
	   }
	  
	   val retVal = List((newStatus, current.head))
	   
	   // Base and recursive cases
	   future match{
	     case Nil => retVal
	     case _   => retVal ++ classifyBatch(nextMedication, future, newStatus)
	   }
	}
	
	
	// Return whether purchase of p2 would overlap with p1
	def overlaps(p1: PurchaseRecord, p2: PurchaseRecord ): Boolean  = {
		val timeDifference = p2.day - p1.day;
		timeDifference < drugDurations(p1.medication)
	}

}



