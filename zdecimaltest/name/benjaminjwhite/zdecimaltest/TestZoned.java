/*
 * TestZoned.java - System Z decimal utility test
 * 
 * Copyright 2007 (c) Benjamin White
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
 * $Date: 2007/08/08 22:45:52 $
 * $Revision: 1.6 $
 * 
 */
package name.benjaminjwhite.zdecimaltest;

import name.benjaminjwhite.zdecimal.*;
import org.junit.*;

/**
 * @author ben
 *
 */
public class TestZoned {
	
	@Test public void longToZone1() throws Exception {
		final byte byta1[] = { (byte) 0xf1 };
		byte byta2[] = ZoneDec.longToZone(1l);
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void longToZone2() throws Exception {
		final byte byta1[] = { (byte) 0xd1 };
		byte byta2[] = ZoneDec.longToZone(-1l);
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void longToZone3() throws Exception {
		final byte byta1[] = {
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				};
		byte byta2[] = ZoneDec.longToZone(1234567890123456789l);
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void longToZone4() throws Exception {
		final byte byta1[] = {
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xd9,
				};
		byte byta2[] = ZoneDec.longToZone(-1234567890123456789l);
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void longToZone5() throws Exception {
		final byte byta1[] = { (byte) 0xf0 };
		byte byta2[] = ZoneDec.longToZone(-0l);
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void longToZone6() throws Exception {
		final byte byta1[] = {
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xd9,
				};
		byte byta2[] = new byte[19];
		ZoneDec.longToZone(-1234567890123456789l, byta2, 0, 19);
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void longToZone7() throws Exception {
		final byte byta1[] = { 0, 0, 
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xd9,
				};
		byte byta2[] = new byte[21];
		ZoneDec.longToZone(-1234567890123456789l, byta2, 2, 19);
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test(expected=DecimalOverflowException.class) public void longToZone8() throws Exception {
		final byte byta1[] = { 0, 0, 
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xd9,
				};
		byte byta2[] = new byte[21];
		ZoneDec.longToZone(-1234567890123456789l, byta2, 2, 18);
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void longToZone9() throws Exception {
		final byte byta1[] = {  
				(byte) 0xf0,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xd9,
				};
		byte byta2[] = new byte[21];
		ZoneDec.longToZone(-1234567890123456789l, byta2, 0, 21);
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void stringToZone1() throws Exception {
		final byte byta1[] = { (byte) 0xf1 }; 
		byte byta2[];
		byta2 = ZoneDec.stringToZone("1");
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void stringToZone2() throws Exception {
		final byte byta1[] = { (byte) 0xd1 }; 
		byte byta2[];
		byta2 = ZoneDec.stringToZone("-1");
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void stringToZone3() throws Exception {
		final byte byta1[] = { (byte) 0xd0 }; 
		byte byta2[];
		byta2 = ZoneDec.stringToZone("-0");
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void stringToZone4() throws Exception {
		final byte byta1[] = { (byte) 0xf3 }; 
		byte byta2[];
		byta2 = ZoneDec.stringToZone("+3");
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test(expected=DataException.class) public void stringToZone5() throws Exception {
		final byte byta1[] = { (byte) 0xf0 }; 
		byte byta2[];
		byta2 = ZoneDec.stringToZone("jj");
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void stringToZone6() throws Exception {
		final byte byta1[] = {
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0
				};
		byte byta2[];
		byta2 = ZoneDec.stringToZone("12345678901234567890");
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void stringToZone7() throws Exception {
		final byte byta1[] = {
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xd0
				};
		byte byta2[];
		byta2 = ZoneDec.stringToZone("-12345678901234567890");
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void stringToZone8() throws Exception {
		final byte byta1[] = {
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xd0
				};
		byte byta2[] = new byte[20];
		ZoneDec.stringToZone("-12345678901234567890",
				byta2, 0, 20 );
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void stringToZone9() throws Exception {
		final byte byta1[] = { 0, 0, 
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xd0
				};
		byte byta2[] = new byte[22];
		ZoneDec.stringToZone("-12345678901234567890",
				byta2, 2, 20 );
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test(expected=DecimalOverflowException.class) public void stringToZone10() throws Exception {
		final byte byta1[] = { 0, 0, 
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xd0
				};
		byte byta2[] = new byte[22];
		ZoneDec.stringToZone("-12345678901234567890",
				byta2, 2, 19 );
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void zoneToLong1() throws Exception {
		final byte byta1[] = {  
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9
				};
		long lnum;
		lnum = ZoneDec.zoneToLong(byta1);
		Assert.assertEquals( 1234567890123456789l, lnum);
	}
	@Test public void zoneToLong2() throws Exception {
		final byte byta1[] = {  
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xd9,
				};
		/* compare to original input */
		final byte byta2[] = {  
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xd9,
				};
		long lnum;
		lnum = ZoneDec.zoneToLong(byta1);
		Assert.assertEquals( -1234567890123456789l, lnum);
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void zoneToLong3() throws Exception {
		final byte byta1[] = {  
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xb9,
				};
		long lnum;
		lnum = ZoneDec.zoneToLong(byta1);
		Assert.assertEquals( -1234567890123456789l, lnum);
	}
	@Test public void zoneToLong4() throws Exception {
		final byte byta1[] = {  
				(byte) 0xf1,
				};
		long lnum;
		lnum = ZoneDec.zoneToLong(byta1);
		Assert.assertEquals( 1l, lnum);
	}
	@Test public void zoneToLong5() throws Exception {
		final byte byta1[] = {  
				(byte) 0xd1,
				};
		long lnum;
		lnum = ZoneDec.zoneToLong(byta1);
		Assert.assertEquals( -1l, lnum);
	}
	@Test public void zoneToLong6() throws Exception {
		final byte byta1[] = {  
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xb9,
				};
		long lnum;
		lnum = ZoneDec.zoneToLong(byta1, 0, 19 );
		Assert.assertEquals( -1234567890123456789l, lnum);
	}
	@Test public void zoneToLong7() throws Exception {
		final byte byta1[] = { 0,  
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xd9,
				0
				};
		long lnum;
		lnum = ZoneDec.zoneToLong(byta1, 1, 19 );
		Assert.assertEquals( -1234567890123456789l, lnum);
	}
	@Test(expected=NumberFormatException.class)
	public void zoneToLong8() throws Exception {
		final byte byta1[] = {   
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf8,
				(byte) 0xf4,
				(byte) 0xf4,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf4,
				(byte) 0xf4,
				(byte) 0xf0,
				(byte) 0xf7,
				(byte) 0xf3,
				(byte) 0xf7,
				(byte) 0xf0,
				(byte) 0xf9,
				(byte) 0xf5,
				(byte) 0xf5,
				(byte) 0xf1,
				(byte) 0xf6,
				(byte) 0xf2,
				(byte) 0xf0
				};
		long lnum;
		lnum = ZoneDec.zoneToLong(byta1, 0, 21 );
		Assert.assertEquals( 4l, lnum);
	}
	@Test public void zoneToString1() throws Exception {
		final byte byta1[] = { 
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xd9
				};
		byte[] byta2 = byta1.clone();
		String snum = ZoneDec.zoneToString(byta1 );
		Assert.assertEquals( "-1234567890123456789", snum );
		Assert.assertArrayEquals(byta1, byta2);
	}
	@Test public void zoneToString2() throws Exception {
		final byte byta1[] = { 
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9
				};
		String snum = ZoneDec.zoneToString(byta1 );
		Assert.assertEquals( "1234567890123456789", snum );
	}
	@Test public void zoneToString3() throws Exception {
		final byte byta1[] = { 
				(byte) 0xf0 
				};
		String snum = ZoneDec.zoneToString(byta1 );
		Assert.assertEquals( "0", snum );
	}
	@Test public void zoneToString4() throws Exception {
		final byte byta1[] = { 0, 
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9, 0
				
				};
		String snum = ZoneDec.zoneToString(byta1, 1, 19 );
		Assert.assertEquals( "1234567890123456789", snum );
	}
	@Test public void zoneToString5() throws Exception {
		final byte byta1[] = { 0, 
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xf9,
				(byte) 0xf0,
				(byte) 0xf1,
				(byte) 0xf2,
				(byte) 0xf3,
				(byte) 0xf4,
				(byte) 0xf5,
				(byte) 0xf6,
				(byte) 0xf7,
				(byte) 0xf8,
				(byte) 0xd9, 0
				
				};
		String snum = ZoneDec.zoneToString(byta1, 1, 19 );
		Assert.assertEquals( "-1234567890123456789", snum );
	}
}
