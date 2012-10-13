/*
 * TestZComp.java - System Z decimal utility test
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
 * $Date:  $
 * $Revision: $
 * 
 */
package name.benjaminjwhite.zdecimaltest;

import name.benjaminjwhite.zdecimal.*;
import org.junit.*;

/**
 * Script for testing name.benjaminjwhite.zdecimal.ZComp
 * @author bjwhite66212
 *
 */

public class TestZComp {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		return;
	}
	
	@Test
	public void  compIntValue1() throws Exception {
		final byte[] ba1 = { 0x00, 0x00, 0x00, 0x00 };
		final byte[] ba2 = ba1.clone();
		final int i1 = 0;
		Assert.assertEquals( i1, ZComp.compIntValue(ba1));
		Assert.assertArrayEquals(ba2, ba1);
	}
	@Test
	public void  compIntValue2() throws Exception {
		final byte[] ba1 = { (byte) 0xa0, 0x12, 0x34, 0x56 };
		final byte[] ba2 = ba1.clone();
		final int i1 = (int) 2685547606l;
		Assert.assertEquals( i1, ZComp.compIntValue(ba1));
		Assert.assertArrayEquals(ba2, ba1);
	}
	@Test
	public void  compIntValue3() throws Exception {
		final byte[] ba1 = { 0x12, 0x34, (byte) 0xb4, 0x78 };
		final byte[] ba2 = ba1.clone();
		final int i1 = 305443960;
		Assert.assertEquals( i1, ZComp.compIntValue(ba1));
		Assert.assertArrayEquals(ba2, ba1);
	}
	@Test
	public void  compShortValue1() throws Exception {
		final byte[] ba1 = { 0x12, (byte) 0xde };
		final byte[] ba2 = ba1.clone();
		final short i1 = 4830;
		Assert.assertEquals( i1, ZComp.compShortValue(ba1));
		Assert.assertArrayEquals(ba2, ba1);
	}
	@Test
	public void shortToBytes1() {
		final short s1 = 400;
		final byte[] ba1 = { 0x01, (byte) 0x90 };
		Assert.assertArrayEquals(ba1, ZComp.shortToBytes(s1));
	}
}
