package name.benjaminjwhite.zdemo;
/*
 * zdemo.java - System Z packed decimal utility demonstration
 * 
 * Copyright 2006 (c) Benjamin White
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * 
 *  */

import name.benjaminjwhite.zdecimal.PackDec;
import name.benjaminjwhite.zdecimal.ZoneDec;

/**
 * 
 * Demonstration program for the zdecimal package.
 * 
 * Remember not to translate the packed or zone decimal numbers when changing code
 * pages.
 * 
 * Most COBOL complires have options that show the offset of each field in 
 * a structure.  Use the listing to determine the field offsets.
 * 
 * JRIO and CICS are registered trademarks of IBM corporation.
 * 
 * @author zdecimal [ at ] benjaminjwhite.name
 * @version 5
 * 
 */
public class Zdemo {	
	/**
	 * 
	 * Runs the ZDecimalJava Demonstration Program
	 * 
	 * @param args NONE
	 * 
	 */
	public static void main(String[] args) throws Exception {
		/*
		 * Let us pretend we have a record on the mainframe with the following COBOL
		 * definition:
		 * 
		 * 01 PART-NUMBER.
		 *    05 PART-NAME        PIC X(20).
		 *    05 PART-NUMBER      PIC 9(5)     USAGE IS COMP-3.
		 *    05 PART-COST        PIC 9(5)V99  USAGE IS COMP-3.
		 *    05 PART-STOCK-LEVEL PIC S9(5)V99 USAGE IS DISPLAY.
		 *    05 FILLER           PIC X(10).
		 * 
		 * We want to double the cost of the part and take three out of stock.
		 * 
		 */
		/* First we create a record for the demo */
		String rec = "Spline series 25A   NNNCCCCLLLLLLLFFFFFFFFFF";
		/*
		 * Convert to a byte array. This is what we would get back from JRIO or a CICS
		 * commarea
		 */
		final String h1 = "      IBM System Z Decimal in Java, Demonstration\n\n  Hexadecimal dump of records and field display (in EBCDIC, Cp1047)\n  Double price and remove 3 from stock\n";
		final String h2 =
				 " 01 PART-NUMBER.\n" +
				 "    05 PART-NAME        PIC X(20).\n"+
				 "    05 PART-NUMBER      PIC 9(5)     USAGE IS COMP-3.\n"+
				 "    05 PART-COST        PIC 9(5)V99  USAGE IS COMP-3.\n"+
				 "    05 PART-STOCK-LEVEL PIC S9(5)V99 USAGE IS DISPLAY.\n"+
				 "    05 FILLER           PIC X(10).\n"
				;
		byte[] recbytes = rec.getBytes("Cp1047");  // Use Z mainframe code page EBCDIC
		// Print headings
		System.out.println(h1);
		System.out.println(h2);
		// insert the part number into the record
		PackDec.stringToPack("33327", recbytes, 20, 3);
		// insert the cost, $17.35 decimal point is implied
		PackDec.longToPack(1735L, recbytes, 23, 4);
		// insert part stock level
		ZoneDec.stringToZone("6800", recbytes, 27, 7);
		System.out.println(new StringBuffer("Our record before: ").append(PackDec.bytesToHex(recbytes)));
		// Print the part number
		System.out.println(new StringBuffer("The part number is: ").append(PackDec.packToLong(recbytes, 20, 3)));
		System.out.println(new StringBuffer("Old Price: ").append(PackDec.packToString(recbytes, 23, 4)));
		System.out.println(new StringBuffer("Old Stock Level: ").append(ZoneDec.zoneToString(recbytes, 27,
				7))); /*
						 * Lets pretend to read the record from the database SQL .....
						 * 
						 * Get the part number from the record as a string
						 * 
						 */
		// Get the current cost
		long cost = PackDec.packToLong(recbytes, 23, 4);
		cost *= 2; // multiply by two
		// Put the nuber back in the record
		PackDec.longToPack(cost, recbytes, 23, 4);
		// Get the current stock level
		long lstock = ZoneDec.zoneToLong(recbytes, 27, 7);
		// Remove three ( and two digits after the decimal point )
		lstock -= 300;
		// Now replace the PART-STOCK-LEVEL field
		ZoneDec.longToZone(lstock, recbytes, 27, 7);
		// Display the record again
		System.out.println(new StringBuffer("\nOur record after:  ").append(PackDec.bytesToHex(recbytes)));
		System.out.println(new StringBuffer("The part number is: ").append(PackDec.packToLong(recbytes, 20, 3)));
		System.out.println(new StringBuffer("New Price: ").append(PackDec.packToString(recbytes, 23, 4)));
		System.out.println(new StringBuffer("New Stock Level: ").append(ZoneDec.zoneToString(recbytes, 27, 7)));
	}

}