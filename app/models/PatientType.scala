package models

object PatientType extends Enumeration
{
	type PatientType 	= Value
	val VIOLATED 		= Value("Patients that violated by taking B and I together.")
	val VALID_NO_COMED 	= Value("Patients that did not violate, because they never took B and I together.")
    val VALID_BI_SWITCH = Value("Patients that did not violate, because they switched from B to I.")
    val VALID_IB_SWITCH = Value("Patients that did not violate, because they switched from I to B.")
    val VALID_I_TRIAL	= Value("Patients that did not violate, because they simply trialled I during B.")
    val VALID_B_TRIAL	= Value("Patients that did not violate, because they simply trialled B during I.")
}