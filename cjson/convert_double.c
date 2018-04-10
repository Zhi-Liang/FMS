#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>
#include <limits.h>
#include <ctype.h>

double c = 0.010000000000000000;
double m = 0.001000000000000000;
double d = 0.100000000000000000;
double dk =10.00000000000000000;
double h = 100.0000000000000000;
double k = 1000.000000000000000;
double hTOs = 3600.000000000000;
double minTOs = 60.000000000000;
double kts = 0.5144444444444444;
double mph = 0.44704;

/* Takes in a string with the units and a value of a variable to be converted.  The units string
 * is compared to the units available in the routine.  If the units are found, then the value is 
 * converted appropriately.  
 * \note All conversions are MKS (meters, kilograms, seconds)
*/
double convert_double (char * input_unit, double variable)
{
//METERS (mm,cm,dm,dkm,hm,km)
 char mmstring[2];
 strcpy(mmstring,"mm");
 char * mmptr = mmstring;

 char cmstring[2];
 strcpy(cmstring,"cm");
 char * cmptr = cmstring;

 char dmstring[2];
 strcpy(dmstring,"dm");
 char * dmptr = dmstring;

 char dkmstring[3];
 strcpy(dkmstring,"dkm");
 char * dkmptr = dkmstring;

 char hmstring[2];
 strcpy(hmstring,"hm");
 char * hmptr = hmstring;

 char kmstring[2];
 strcpy(kmstring,"km");
 char * kmptr = kmstring;

//GRAMS (mg,cg,dg,g,dkg,hg,kg)
// NOTE: these are converted to kg, not g (mks)
 char mgstring[2];
 strcpy(mgstring,"mg");
 char * mgptr = mgstring;

 char cgstring[2]="cg";
 strcpy(cgstring,"cg");
 char * cgptr = cgstring;

 char dgstring[2];
 strcpy(dgstring,"dg");
 char * dgptr = dgstring;

 char gstring[1];
 strcpy(gstring,"g");
 char * gptr = gstring;

 char dkgstring[3];
 strcpy(dkgstring,"dkg");
 char * dkgptr = dkgstring;

 char hgstring[2];
 strcpy(hgstring,"hg");
 char * hgptr = hgstring;

 char kgstring[2];
 strcpy(kgstring,"kg");
 char * kgptr = kgstring;

//PASCALS (mPa,cPa,dPa,dkPa,hPa,kPa)
 char mPastring[3];
 strcpy(mPastring,"mPa");
 char * mPaptr = mPastring;

 char cPastring[3]="cPa";
 strcpy(cPastring,"cPa");
 char * cPaptr = cPastring;

 char dPastring[3];
 strcpy(dPastring,"dPa");
 char * dPaptr = dPastring;

 char dkPastring[4];
 strcpy(dkPastring,"dkPa");
 char * dkPaptr = dkPastring;

 char hPastring[3];
 strcpy(hPastring,"hPa");
 char * hPaptr = hPastring;
 char mbstring[2];
 strcpy(mbstring,"mb");
 char * mbptr = mbstring;

 char kPastring[3];
 strcpy(kPastring,"kPa");
 char * kPaptr = kPastring;

//JOULES (mJ,cJ,dJ,dkJ,hJ,kJ)
 char mJstring[2];
 strcpy(mJstring,"mJ");
 char * mJptr = mJstring;

 char cJstring[2]="cJ";
 strcpy(cJstring,"cJ");
 char * cJptr = cJstring;

 char dJstring[2];
 strcpy(dJstring,"dJ");
 char * dJptr = dJstring;

 char dkJstring[3];
 strcpy(dkJstring,"dkJ");
 char * dkJptr = dkJstring;

 char hJstring[2];
 strcpy(hJstring,"hJ");
 char * hJptr = hJstring;

 char kJstring[2];
 strcpy(kJstring,"kJ");
 char * kJptr = kJstring;

// TIME (min,h or hours)
 char hourstring[1];
 strcpy(hourstring,"h");
 char * hourptr = hourstring;

 char hourSstring[5];
 strcpy(hourSstring,"hours");
 char * hourSptr = hourSstring;

 char minstring[3];
 strcpy(minstring,"min");
 char * minptr = minstring;

// SPEED (km h-1, kts, knots, mph, cm/s, cm/h)
 char kmhstring[6];
 strcpy(kmhstring,"km h-1");
 char * kmhptr = kmhstring;
 char kmperhstring[6];
 strcpy(kmperhstring,"km/h");
 char * kmperhptr = kmperhstring;

 char ktsstring[3];
 strcpy(ktsstring,"kts");
 char * ktsptr = ktsstring;
 char knotsstring[6];
 strcpy(knotsstring,"knots");
 char * knotsptr = knotsstring;

 char mphstring[3];
 strcpy(mphstring,"mph");
 char * mphptr = mphstring;
 char mihstring[6];
 strcpy(mihstring,"mi h-1");
 char * mihptr = mihstring;
 char miphstring[6];
 strcpy(miphstring,"mi/h");
 char * miphptr = miphstring;

 char cmhstring[6];
 strcpy(cmhstring,"cm h-1");
 char * cmhptr = cmhstring;
 char cmperhstring[6];
 strcpy(cmperhstring,"cm/h");
 char * cmperhptr = cmperhstring;

 char cmsstring[6];
 strcpy(cmsstring,"cm s-1");
 char * cmsptr = cmsstring;
 char cmpersstring[6];
 strcpy(cmpersstring,"cm/s");
 char * cmpersptr = cmpersstring;

/* ********************************************************************************************* */
// The more common unit conversions
/* hours or h to s */
 if (strcmp(input_unit,hourptr)==0 || strcmp(input_unit,hourSptr)==0){
    return variable*hTOs;
                                                                     }
/* hPa or mb to Pa */
 if (strcmp(input_unit,hPaptr)==0 || strcmp(input_unit,mbptr)==0){
    return variable*h;
                                  }
/* km to m */                                 
 if (strcmp(input_unit,kmptr)==0){
    return variable*k;
                                 }
/* kJ to J */                                 
 if (strcmp(input_unit,kJptr)==0){
    return variable*k;
                                 }
/* km/h to m/s */                                 
 if (strcmp(input_unit,kmhptr)==0 || strcmp(input_unit,kmperhptr)==0){
    return variable*k/hTOs;
                                 }
/* g to kg */
 if (strcmp(input_unit,gptr)==0){
    return variable/k; //NOTE divided by k, not times
                                 }
/* dkm to m */                                 
 if (strcmp(input_unit,dkmptr)==0){
    return variable*dk;
                                 }
/* cm to m */                                 
 if (strcmp(input_unit,cmptr)==0){
    return variable*c;
                                 }
/* cm/s to m/s */                                 
 if (strcmp(input_unit,cmsptr)==0 || strcmp(input_unit,cmpersptr)==0){
    return variable*c;
                                 }
/* cm/h to m/s */                                 
 if (strcmp(input_unit,cmhptr)==0 || strcmp(input_unit,cmperhptr)==0){
    return variable*c/hTOs;
                                 }
/* knots to m/s */                                 
 if (strcmp(input_unit,ktsptr)==0 || strcmp(input_unit,knotsptr)==0){
    return variable*kts;
                                 }
/* mph to m/s */                                 
 if (strcmp(input_unit,mphptr)==0 || strcmp(input_unit,miphptr)==0 || strcmp(input_unit,mihptr)==0){
    return variable*mph;
                                 }
/* The rest of the conversions are below. */
/* meters */                                 
 if (strcmp(input_unit,hmptr)==0){
    return variable*h;
                                 }
 if (strcmp(input_unit,dmptr)==0){
    return variable*d;
                                 }
 if (strcmp(input_unit,mmptr)==0){
    return variable*m;
                                 }
/* grams */                                 
 if (strcmp(input_unit,hgptr)==0){
    return variable/10;
                                 }
 if (strcmp(input_unit,dkgptr)==0){
    return variable/100;
                                 }
 if (strcmp(input_unit,dgptr)==0){
    return variable/10000;
                                 }
 if (strcmp(input_unit,cgptr)==0){
    return variable/100000;
                                 }
 if (strcmp(input_unit,mgptr)==0){
    return variable/1000000;
                                  }
/* Pasacls */                                 
 if (strcmp(input_unit,kPaptr)==0){
    return variable*k;
                                  }
 if (strcmp(input_unit,dkPaptr)==0){
    return variable*dk;
                                   }
 if (strcmp(input_unit,dPaptr)==0){
    return variable*d;
                                  }
 if (strcmp(input_unit,cPaptr)==0){
    return variable*c;
                                  }
 if (strcmp(input_unit,mPaptr)==0){
    return variable*m;
                                  }
/* Joules */                                 
 if (strcmp(input_unit,hJptr)==0){
    return variable*h;
                                 }
 if (strcmp(input_unit,dkJptr)==0){
    return variable*dk;
                                  }
 if (strcmp(input_unit,dJptr)==0){
    return variable*d;
                                 }
 if (strcmp(input_unit,cJptr)==0){
    return variable*c;
                                 }
 if (strcmp(input_unit,mJptr)==0){
    return variable*m;
                                 }
/* Time */                                 
 if (strcmp(input_unit,minptr)==0 ){
    return variable*minTOs;
                                    }
                                                                     

 return variable*1.000000000000000000000000000000;
}
/* ********************************************************************************************* */
/* ********************************************************************************************* */


double convert_check_dble (char * input_unit,double variable)
{
//mks units
 char kgstring[3];
 strcpy(kgstring,"kg");
 char * kgptr = kgstring;
 char mstring[2];
 strcpy(mstring,"m");
 char * mptr = mstring;
 char sstring[2];
 strcpy(sstring,"s");
 char * sptr = sstring;
 char Pastring[3];
 strcpy(Pastring,"Pa");
 char * Paptr = Pastring;
 char mpsstring[4];
 strcpy(mpsstring,"m/s");
 char * mpsptr = mpsstring;
 char ms1string[6];
 strcpy(ms1string,"m s-1");
 char * ms1ptr = ms1string; 
 if (strcmp(input_unit,kgptr)==0 || strcmp(input_unit,mptr)==0 || strcmp(input_unit,sptr)==0 || strcmp(input_unit,Paptr)==0 || strcmp(input_unit,mpsptr)==0 || strcmp(input_unit,ms1ptr)==0){
    return -2.0;
                                                                     }
 

//METERS (mm,cm,dm,dkm,hm,km)
 char mmstring[3];
 strcpy(mmstring,"mm");
 char * mmptr = mmstring;

 char cmstring[3];
 strcpy(cmstring,"cm");
 char * cmptr = cmstring;

 char dmstring[3];
 strcpy(dmstring,"dm");
 char * dmptr = dmstring;

 char dkmstring[4];
 strcpy(dkmstring,"dkm");
 char * dkmptr = dkmstring;

 char hmstring[3];
 strcpy(hmstring,"hm");
 char * hmptr = hmstring;

 char kmstring[3];
 strcpy(kmstring,"km");
 char * kmptr = kmstring;

//GRAMS (mg,cg,dg,g,dkg,hg,kg)
// NOTE: these are converted to kg, not g (mks)
 char mgstring[3];
 strcpy(mgstring,"mg");
 char * mgptr = mgstring;

 char cgstring[3]="cg";
 strcpy(cgstring,"cg");
 char * cgptr = cgstring;

 char dgstring[3];
 strcpy(dgstring,"dg");
 char * dgptr = dgstring;

 char gstring[2];
 strcpy(gstring,"g");
 char * gptr = gstring;

 char dkgstring[4];
 strcpy(dkgstring,"dkg");
 char * dkgptr = dkgstring;

 char hgstring[3];
 strcpy(hgstring,"hg");
 char * hgptr = hgstring;


//PASCALS (mPa,cPa,dPa,dkPa,hPa,kPa)
 char mPastring[4];
 strcpy(mPastring,"mPa");
 char * mPaptr = mPastring;

 char cPastring[4]="cPa";
 strcpy(cPastring,"cPa");
 char * cPaptr = cPastring;

 char dPastring[4];
 strcpy(dPastring,"dPa");
 char * dPaptr = dPastring;

 char dkPastring[5];
 strcpy(dkPastring,"dkPa");
 char * dkPaptr = dkPastring;

 char hPastring[4];
 strcpy(hPastring,"hPa");
 char * hPaptr = hPastring;
 char mbstring[3];
 strcpy(mbstring,"mb");
 char * mbptr = mbstring;

 char kPastring[4];
 strcpy(kPastring,"kPa");
 char * kPaptr = kPastring;

//JOULES (mJ,cJ,dJ,dkJ,hJ,kJ)
 char mJstring[3];
 strcpy(mJstring,"mJ");
 char * mJptr = mJstring;

 char cJstring[3]="cJ";
 strcpy(cJstring,"cJ");
 char * cJptr = cJstring;

 char dJstring[3];
 strcpy(dJstring,"dJ");
 char * dJptr = dJstring;

 char dkJstring[4];
 strcpy(dkJstring,"dkJ");
 char * dkJptr = dkJstring;

 char hJstring[3];
 strcpy(hJstring,"hJ");
 char * hJptr = hJstring;

 char kJstring[3];
 strcpy(kJstring,"kJ");
 char * kJptr = kJstring;

// TIME (min,h or hours)
 char hourstring[2];
 strcpy(hourstring,"h");
 char * hourptr = hourstring;

 char hourSstring[5];
 strcpy(hourSstring,"hours");
 char * hourSptr = hourSstring;

 char minstring[4];
 strcpy(minstring,"min");
 char * minptr = minstring;

// SPEED (km h-1, kts, knots, mph, cm/s, cm/h)
 char kmhstring[7];
 strcpy(kmhstring,"km h-1");
 char * kmhptr = kmhstring;
 char kmperhstring[5];
 strcpy(kmperhstring,"km/h");
 char * kmperhptr = kmperhstring;

 char ktsstring[4];
 strcpy(ktsstring,"kts");
 char * ktsptr = ktsstring;
 char knotsstring[6];
 strcpy(knotsstring,"knots");
 char * knotsptr = knotsstring;

 char mphstring[4];
 strcpy(mphstring,"mph");
 char * mphptr = mphstring;
 char mihstring[7];
 strcpy(mihstring,"mi h-1");
 char * mihptr = mihstring;
 char miphstring[5];
 strcpy(miphstring,"mi/h");
 char * miphptr = miphstring;

 char cmhstring[7];
 strcpy(cmhstring,"cm h-1");
 char * cmhptr = cmhstring;
 char cmperhstring[5];
 strcpy(cmperhstring,"cm/h");
 char * cmperhptr = cmperhstring;

 char cmsstring[7];
 strcpy(cmsstring,"cm s-1");
 char * cmsptr = cmsstring;
 char cmpersstring[5];
 strcpy(cmpersstring,"cm/s");
 char * cmpersptr = cmpersstring;

/* ********************************************************************************************* */
// The more common unit conversions
/* hours or h to s */
 if (strcmp(input_unit,hourptr)==0 || strcmp(input_unit,hourSptr)==0){
    return 1.0;
                                                                     }
/* hPa or mb to Pa */
 if (strcmp(input_unit,hPaptr)==0 || strcmp(input_unit,mbptr)==0){
    return 1.0;
                                  }
/* km to m */                                 
 if (strcmp(input_unit,kmptr)==0){
    return 1.0;
                                 }
/* kJ to J */                                 
 if (strcmp(input_unit,kJptr)==0){
    return 1.0;
                                 }
/* km/h to m/s */                                 
 if (strcmp(input_unit,kmhptr)==0 || strcmp(input_unit,kmperhptr)==0){
    return 1.0;
                                 }
/* g to kg */
 if (strcmp(input_unit,gptr)==0){
    return 1.0;
                                 }
/* dkm to m */                                 
 if (strcmp(input_unit,dkmptr)==0){
    return 1.0;
                                 }
/* cm to m */                                 
 if (strcmp(input_unit,cmptr)==0){
    return 1.0;
                                 }
/* cm/s to m/s */                                 
 if (strcmp(input_unit,cmsptr)==0 || strcmp(input_unit,cmpersptr)==0){
    return 1.0;
                                 }
/* cm/h to m/s */                                 
 if (strcmp(input_unit,cmhptr)==0 || strcmp(input_unit,cmperhptr)==0){
    return 1.0;
                                 }
/* knots to m/s */                                 
 if (strcmp(input_unit,ktsptr)==0 || strcmp(input_unit,knotsptr)==0){
    return 1.0;
                                 }
/* mph to m/s */                                 
 if (strcmp(input_unit,mphptr)==0 || strcmp(input_unit,miphptr)==0 || strcmp(input_unit,mihptr)==0){
    return 1.0;
                                 }
/* The rest of the conversions are below. */
/* meters */                                 
 if (strcmp(input_unit,hmptr)==0){
    return 1.0;
                                 }
 if (strcmp(input_unit,dmptr)==0){
    return 1.0;
                                 }
 if (strcmp(input_unit,mmptr)==0){
    return 1.0;
                                 }
/* grams */                                 
 if (strcmp(input_unit,hgptr)==0){
    return 1.0;
                                 }
 if (strcmp(input_unit,dkgptr)==0){
    return 1.0;
                                 }
 if (strcmp(input_unit,dgptr)==0){
    return 1.0;
                                 }
 if (strcmp(input_unit,cgptr)==0){
    return 1.0;
                                 }
 if (strcmp(input_unit,mgptr)==0){
    return 1.0;
                                  }
/* Pasacls */                                 
 if (strcmp(input_unit,kPaptr)==0){
    return 1.0;
                                  }
 if (strcmp(input_unit,dkPaptr)==0){
    return 1.0;
                                   }
 if (strcmp(input_unit,dPaptr)==0){
    return 1.0;
                                  }
 if (strcmp(input_unit,cPaptr)==0){
    return 1.0;
                                  }
 if (strcmp(input_unit,mPaptr)==0){
    return 1.0;
                                  }
/* Joules */                                 
 if (strcmp(input_unit,hJptr)==0){
    return 1.0;
                                 }
 if (strcmp(input_unit,dkJptr)==0){
    return 1.0;
                                  }
 if (strcmp(input_unit,dJptr)==0){
    return 1.0;
                                 }
 if (strcmp(input_unit,cJptr)==0){
    return 1.0;
                                 }
 if (strcmp(input_unit,mJptr)==0){
    return 1.0;
                                 }
/* Time */                                 
 if (strcmp(input_unit,minptr)==0 ){
    return 1.0;
                                    }
                                                                     

 return -1.0;
}














