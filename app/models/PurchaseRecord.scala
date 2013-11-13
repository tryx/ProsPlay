package models

class PurchaseRecord(day: Int, medication: String, patientID: Int)
{
	override def toString() = "day: %d, medication: %s, patient: %d" format (day, medication, patientID);  
}

object PurchaseRecord {
  	val MOCK_DATA_PATH = "./public/data.csv";
	def loadFromFile: List[PurchaseRecord] = 
	{
  		val lines = io.Source.fromFile(MOCK_DATA_PATH).getLines;
  		lines.map {_ split ',' match { 
  		  			   case Array(day, med, patient) => new PurchaseRecord(day.toInt, med, patient.toInt)
  		  			 }}
  		  	 .toList 
	}
  
}