# bp

### Background
Cardiovascular (CV) disease is the leading cause of death worldwide 1 with hypertension, specifically, affecting over 1.1 billion people annually. With such grave implications, one would imagine that there would be ample resources to analyze such data in R; surprisingly however, there are currently no R packages for such analysis. 

Blood pressure monitoring devices work by measuring the pressures of the artery’s restricted blood flow; for digital devices, the vibrations are translated into electrical signals With recent technological advancements, ambulatory blood pressure monitoring (ABPM) devices offer a unique solution that take automatic readings at pre-specified intervals. ABPM allows medical professionals to analyze blood pressure during sleep which has been shown to be a more accurate predictor of CV events than daytime blood pressure. ABPM also allows researchers to discern true hypertension from "whitecoat" hypertension in an office or laboratory setting. The goal of this bp package is to provide a comprehensive tool that allows researchers to analyze blood pressure data using a variety of statistical methods and metrics.

### Intended Functionality
The bp package will allow the user to specify any combination of the following time-indexed variables from the supplied data set (with the minimum requirement that SBP and DBP are included) and leverage them for further analysis:
• Systolic Blood Pressure (SBP) measured in mmHg
• Diastolic Blood Pressure (DBP) measured in mmHg
• Heart Rate (HR) measured in bpm
• Pulse Pressure (PP) measured in mmHg
• Mean Arterial Pressure (MAP) measured in mmHg
• Rate Pressure Product (RPP) which is calculated as SBP multiplied by resting HR

The package will then utilize the above variables to calculate various metrics from medical and statistical literature (ARV, morning surge %, etc) in order to quantify the variability of the readings, classify subjects as either "dipper" or "non-dipper", and cluster subjects (if more than one) into their respective categories of hypertension (normal, elevated, or hypertensive).
