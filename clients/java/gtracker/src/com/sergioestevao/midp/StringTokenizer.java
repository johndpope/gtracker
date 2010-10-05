/**
 * Sergio Estevao
 * MIDP Adventures
 * www.sergioestevao.com/midp
 */
package com.sergioestevao.midp;

public class StringTokenizer {

  private String msg;

  private String delimiters;

  public StringTokenizer(String aMsg, String aDelimiters) {
    setString(aMsg);
    setTokens(aDelimiters);
  }

  public void setString(String aMsg) {
    msg = aMsg;
  }

  public void setTokens(String aDelimiters) {
    delimiters = aDelimiters;
  }

  public boolean hasMoreTokens() {
    return msg != null;
  }

  // return next token (filters delimiter)
  public String nextToken() {
    String t = msg.substring(0, getPos());
    if (!t.equals(msg)) {
      msg = msg.substring(getPos() + 1);
    } else {
      msg = null;
    }
    return t;
  }

  // return position of next delimiter, -1 means no more delimiters
  // available
  private int getPos() {

    int currentPos = msg.length();
    int newPos = -1;
    for (int i = 0; i < delimiters.length(); i++) {
      newPos = msg.indexOf(delimiters.charAt(i));
      if (newPos != -1) {
        if (currentPos != -1) {
          currentPos = Math.min(currentPos, newPos);
        } else {
          currentPos = newPos;
        }
      }
    }
    return currentPos;
  }

}
