/*
 * ZoneDec.java - System Z decimal utility
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
 * $Author: bjwhite66212 $
 * 
 */

package name.benjaminjwhite.zdecimal;

import java.io.UnsupportedEncodingException;

/**
 * Utility class to handle IBM System Z zone decimal data in Java. All zone
 * decimal number operands must be valid format with valid sign. Java long will
 * not hold the maximum size number. java.math.BigDecimal can be used to handle
 * larger numbers than will fit in java long.
 * 
 * "Zone decimal" format is a method of representing numbers in the IBM System Z 
 * computers. It was also present in the 360, 370 and 390 series. Decimal digits
 * are stored as lower 4 bits of each byte. The upper 4 bits are binary 1111
 * except for the last byte. The first four bits are the last byte is the sign
 * of the number. Positive are binary 1010, 1100 1110 and 1111. Negative is 1011
 * and 1101.
 * 
 * COBOL is a popular mainframe language that has a number format with a "USAGE"
 * of "DISPLAY". When a "USAGE DISPLAY" number has a sign on the picture, the
 * number is stored a s zone decimal.
 * 
 * An example: <code><pre>
 *    01 PART-NUMBER.
 *       05  PART-NAME                   PIC X(20).
 *       05  PART-NUMBER                 PIC 9(5) USAGE IS COMP-3.
 *       05  PART-COST                   PIC 9(5)V99 USAGE IS COMP-3.
 *       05  PART-STOCK-LEVEL            PIC S9(5)V99 USAGE IS DISPLAY.
 *       05  FILLER                      PIC X(10).
 * </pre></code>
 * 
 * The PART-STOCK-LEVEL would be stored in memory as zone decimal and occupy 7
 * bytes. The implied decimal does not reserve any storage
 * 
 * <br/> Z/Series, 360, 370 and 390 is a trademark of IBM Corporation
 * @see <a href="http://www.ibm.com">IBM.COM</a>
 * 
 * @see <a href="http://benjaminjwhite.name/zdecimal">Z Decimal for java project home page</a>
 * 
 * @author zdecimal [ at ] benjaminjwhite.name
 * @version 4
 * 
 */
public class ZoneDec {
	/**
	 * Converts Java long to zone decimal
	 * 
	 * @param lnum
	 *            long number
	 * @return byte array of zone decimal
	 * @throws UnsupportedEncodingException Invalid codepage
	 */
	public static byte[] longToZone(long lnum)
			throws UnsupportedEncodingException {
		String str = new Long(lnum).toString();
		if (lnum < 0)
			str = str.substring(1);
		byte[] bytes = str.getBytes("Cp1047");
		if (lnum < 0) {
			int i = bytes.length - 1;
			bytes[i] = (byte) ((byte) (bytes[i] & 0x0f) | 0xd0);
			return (bytes);
		} else {
			return (bytes);
		}

	}

	/**
	 * Convert long to zone decimal and store in byte array
	 * 
	 * @param lnum
	 *            Number to convert
	 * @param bytearray
	 *            Target byte array
	 * @param offset
	 *            Into byte array
	 * @param len
	 *            Length of field in byte array
	 * @throws UnsupportedEncodingException Invalid code page
	 * @throws DecimalOverflowException Number will not fit in field.
	 */
	public static void longToZone(long lnum, byte[] bytearray, int offset,
			int len) throws UnsupportedEncodingException,
			DecimalOverflowException {
		int i;
		int j;
		int k;
		j = offset;
		for (i = 0; i < len; i++, j++)
			bytearray[j] = -16; // zero output array to 0xf0
		byte[] wrkba = longToZone(lnum);
		int wrkbalen = wrkba.length;
		if (wrkbalen > len)
			throw new DecimalOverflowException("Number too large: " + lnum );
		i = offset + len - 1;
		j = wrkbalen - 1;
		for (k = 0; k < wrkbalen; k++, i--, j--)
			bytearray[i] = wrkba[j];
	}

	/**
	 * Converts a java String to System Z zoned decimal
	 * 
	 * @param znstr
	 *            input number
	 * @return byte array of Zone decimal number
	 * @throws DataException
	 *             Invalid characters
	 * @throws UnsupportedEncodingException Invalid code page
	 */
	public static byte[] stringToZone(String znstr) throws DataException,
			UnsupportedEncodingException {
		boolean isminus = false;
		String wkznstr = znstr;
		int i;
		int strlen;
		byte znbytes[];
		String firstch = wkznstr.substring(0, 1);
		if (firstch.equals("+")) // handle sign
			wkznstr = wkznstr.substring(1);
		else if (firstch.equals("-")) {
			wkznstr = wkznstr.substring(1);
			isminus = true;
		}
		strlen = wkznstr.length();
		for (i = 0; i < strlen; i++) {
			if (!Character.isDigit(wkznstr.charAt(i))) {
				throw new DataException("Invalid Digit: " + wkznstr.charAt(i) +
						" in " + znstr );
			}
		}
		znbytes = wkznstr.getBytes("Cp1047");
		if (isminus) {
			i = strlen - 1;
			znbytes[i] = (byte) (znbytes[i] & 0x0f);
			znbytes[i] = (byte) (znbytes[i] | 0xd0);
		}
		return (znbytes);
	}

	/**
	 * Converts java.lang.String to zone decimal and store in byte array
	 * 
	 * @param str
	 *            Input string
	 * @param bytearray
	 *            Output byte array
	 * @param offset
	 *            Into output byte array
	 * @param len
	 *            of sub array
	 * @throws DecimalOverflowException
	 *             Input number too big for output
	 * @throws DataException
	 *             Invalid characters
	 * @throws UnsupportedEncodingException
	 *             Invalid code page
	 */
	public static void stringToZone(String str, byte[] bytearray, int offset,
			int len) throws DecimalOverflowException, DataException,
			UnsupportedEncodingException {
		String wrkstr;
		char firstchar = str.charAt(0);
		if ('-' == firstchar || '+' == firstchar)
			if (str.length() < 2)
				throw new DataException("Invalid character" + firstchar + " in "
						+ str );
			else
				wrkstr = firstchar + "000000000000000" + str.substring(1);
		else
			wrkstr = "000000000000000" + str;
		byte[] znnum = stringToZone(wrkstr); // get zoned number
		int znlen = znnum.length;
		if (len < znlen) // check to see of number too big
			if (znnum[znlen - len - 1] != -16) // -16 is 0xf0
				throw new DecimalOverflowException("Number too big: " + str );
		int i = offset + len - 1;
		int j = znlen - 1;
		int k;
		for (k = 0; k < len; k++) {
			bytearray[i] = znnum[j];
			--i;
			--j;
		}
	}

	/**
	 * Convert byte array containing zone decimal number
	 * 
	 * @param bytearrayi
	 *            input number
	 * @return long number
	 * @throws UnsupportedEncodingException
	 *             Invalid code page
	 */
	public static long zoneToLong(byte[] bytearrayi)
			throws UnsupportedEncodingException {
		boolean isminus = false;
		String str;
		long lnum;
		byte [] bytearray = bytearrayi.clone();
		int i = bytearray.length - 1;
		int b1 = (bytearray[i] & 0xf0) >>> 4;
		if (11 == b1 || 13 == b1)
			isminus = true;
		bytearray[i] = (byte) (bytearray[i] | 0xf0);
		str = new String(bytearray, "Cp1047");
		lnum = Long.parseLong(str);
		if (isminus)
			lnum *= -1L;
		return (lnum);
	}

	/**
	 * Convert selected bytes of array of zone decimal format to long
	 * 
	 * @param bytearray
	 *            Input zone decimal number
	 * @param offset
	 *            Into byte array
	 * @param len
	 *            Length of byte array field
	 * @return long number
	 * @throws UnsupportedEncodingException
	 */
	public static long zoneToLong(byte[] bytearray, int offset, int len)
			throws UnsupportedEncodingException {
		byte[] wkba = new byte[len];
		int i = offset;
		int j;
		for (j = 0; j < len; j++, i++)
			wkba[j] = bytearray[i];
		return (zoneToLong(wkba));
	}

	/**
	 * Converts a byte array of zoned decimal to a string
	 * 
	 * @param znbytesi
	 *            zone decimal byte array
	 * @return Java String of number
	 * @throws UnsupportedEncodingException
	 *             Invalid code page
	 */
	public static String zoneToString(byte[] znbytesi)
			throws UnsupportedEncodingException {
		String str;
		int b1;
		int i;
		byte [] znbytes = znbytesi.clone();
		int znlen = znbytes.length;
		i = znlen - 1;
		b1 = znbytes[i];
		b1 = (b1 & 0xf0) >>> 4;
		switch (b1) {
		case 13:
		case 10:
			znbytes[i] = (byte) (znbytes[i] | 0xf0);
			str = "-" + new String(znbytes, "Cp1047");
			break;
		default:
			znbytes[i] = (byte) (znbytes[i] | 0xf0);
			str = new String(znbytes, "Cp1047");
		}
		return (str);
	}

	/**
	 * Converts byte array of zone decimal to string
	 * 
	 * @param znbytes
	 *            byte array zone decimal number
	 * @param offset
	 *            Into byte array
	 * @param len
	 *            Length of zone decimal field
	 * @return Java String of number
	 * @throws UnsupportedEncodingException
	 *             Invalid code page
	 */
	public static String zoneToString(byte[] znbytes, int offset, int len)
			throws UnsupportedEncodingException {
		byte[] bytearray = new byte[len];
		int i;
		int j = offset;
		for (i = 0; i < len; i++)
			bytearray[i] = znbytes[j++];
		return (zoneToString(bytearray));
	}
}
