/*
 * ZComp.java - IBM System Z Java binary number utility
 * 
 * Copyright 2012 (c) Benjamin White
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
 * $Author: bjwhite66212 $
 * $Date: $
 *
 * $Revision: $
 * 
 */
/**
 * Utility class to convert byte arrays representing binary numbers to and from Java short
 * and int.
 * 
 * <br/> System Z, Z/Series, 360, 370 and 390 is a trademark of IBM Corporation
 * @see <a href="http://www.ibm.com">IBM.COM</a>
 * 
 * @see <a href="http://benjaminjwhite.name/zdecimal">Z Decimal for java project home page</a>
 * 
 * @author zdecimal [ at ] benjaminjwhite.name
 * @version 4
 * 
 */
package name.benjaminjwhite.zdecimal;

/**
 * @author bjwhite66212
 * 
 */
public class ZComp {
	/**
	 * Converts byte array to short, unsigned numbers
	 * 
	 * @param ba - byte array 2 long, COBOL PIC 9(1 to 4) USAGE IS COMPUTATIONAL
	 * @return short value
	 * @throws FixedPointDivideException
	 */
	public static short compShortValue(byte[] ba)
			throws FixedPointDivideException {
		if (ba.length != 2)
			throw new FixedPointDivideException(
					"bytee array length must be 2, but is: " + ba.length);
		int i;
		i = ba[0] & 0xff;
		i = i << 8;
		i += ba[1] & 0xff;
		return ((short) i);
	}
/**
 * Convert byte array to int, unsigned numbers
 * 
 * @param ba - byte array 4 long, COBOL PIC 9(5 to 9) USAGE IS COMPUTATIONAL
 * @return int value
 * @throws FixedPointDivideException
 */
	public static int compIntValue(byte[] ba) throws FixedPointDivideException {
		if (ba.length != 4)
			throw new FixedPointDivideException(
					"byte array length must be 4, but is: " + ba.length);
		long i = ba[0] & 0xff ;
		i = i << 8;		
		i += ba[1] & 0xff;
		i = i << 8;
		i += ba[2] & 0xff;
		i = i << 8;
		i += ba[3] & 0xff;
		return ((int) i);
	}
/**
 * Convert short to byte array 2 long
 * @param s - short number
 * @return byte array
 */
	public static byte[] shortToBytes(short s) {
		byte[] ba = { (byte) (s >>> 8), (byte) s };
		return (ba);
	}
/**
 * Convert int to byte array 4 long
 * @param i - int
 * @return byte array
 */
	public static byte[] intToBytes(int i) {
		byte[] ba = { (byte) (i >>> 24), (byte) (i >>> 16), (byte) (i >>> 8),
				(byte) i };
		return (ba);
	}
}
