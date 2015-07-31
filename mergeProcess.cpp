#include <Rcpp.h>
#include <iostream>
#include <exception>
#include <string>
#include <exception>
#include <cstring>
#include <fstream>
#include <vector>
#include <math.h>

#include "/home/lisa/rDia/lib/csv.h"

using namespace Rcpp;
using namespace io;
using namespace std;

// Enable C++11
// [[Rcpp::plugins("cpp11")]]

const int packetLen = 20;

// packet
struct PacketStr {
  int    frameNumber;
	double frameTimeRel;
	double frameTimeDelta;
	int    frameLen;
	int    ipProto;
	int    ipVersion;
	string ipSrc;
	string ethSrc;
	string ipDst;
	string ethDst;
	int    modbusUnitId;
	int    tcpSrcPort;
	int    tcpDstPort;
	int    mbtcpProtId;
	int    mbtcpTransId;
	int    mbtcpLen;
	int    modbusFunctionCode;
	int   modbusRefNum;
	int    modbusWordCnt;
	string modbusData;
} packet, prevPacket;

// merged mbtcp transaction
struct MbtcpTransStr {
  int    frameNumber;
  double frameTimeRel;
  double frameTimeDelta;
  int    frameLen;
  int    ipProto;
  int    ipVersion;
  string ipSrc;
  string ethSrc;
  string ipDst;
  string ethDst;
  int    modbusUnitId;
  int    tcpSrcPort;
  int    tcpDstPort;
	int    mbtcpProtId;
	int    mbtcpTransId;
	int    mbtcpLen;
	int    modbusFunctionCode;
	int    modbusRefNum;
	int    modbusWordCnt;
	int    frameSecond;
  int    respframeNumber;
	double respTimeRel;
	double respTimeDelta;
	int    respLen;
	string respIpSrc;
	string respEthSrc;
	string respIpDest;
	string respEthDest;
	int    respUnitId;
	int    respSrcport;
	int    respDstPort;
	int    respProtId;
	int    respTransId;
	int    respMbtcpLen;
	int    respFuncCode;
	string modbusData;
	int    respSecond;
	int    d;
} mbtcpTrans;

// all merged mbtcp transactions
int nrows = 24945; // change to input parm
IntegerVector frNum(nrows);
NumericVector frameTimeRel(nrows);
NumericVector frameTimeDelta(nrows);
IntegerVector frameLen(nrows);
IntegerVector ipProto(nrows);
IntegerVector ipVersion(nrows);
CharacterVector ipSrc(nrows);
CharacterVector ethSrc(nrows);
CharacterVector ipDst(nrows);
CharacterVector ethDst(nrows);
IntegerVector modbusUnitId(nrows);
IntegerVector tcpSrcPort(nrows);
IntegerVector tcpDstPort(nrows);
IntegerVector mbtcpProtId(nrows);
IntegerVector mbtcpTransId(nrows);
IntegerVector mbtcpLen(nrows);
IntegerVector modbusFunctionCode(nrows);
IntegerVector modbusRefNum(nrows);
IntegerVector modbusWordCnt(nrows);
CharacterVector modbusData(nrows);

IntegerVector respFrameNumber(nrows);
NumericVector respTimeRel(nrows);
NumericVector respTimeDelta(nrows);
IntegerVector respLen(nrows);
CharacterVector respIpSrc(nrows);
CharacterVector respEthSrc(nrows);
CharacterVector respIpDst(nrows);
CharacterVector respEthDst(nrows);
IntegerVector respUnitId(nrows);
IntegerVector respSrcPort(nrows);
IntegerVector respDstPort(nrows);
IntegerVector respProtId(nrows);
IntegerVector respTransId(nrows);

// ********************* FUNCTIONS ********************* 

// convert hexadecimal string to decimal
int hexToDec(string str) {

  string decS;
	char delim[] = ":";
	char *token = strtok( &str[0], delim );

	while (token != NULL ) {

		decS += token;
		token = strtok( NULL, delim );
	}

	return strtol( decS.c_str(), NULL, 16 );
}

// merge transactions from pcap file
int mergeTrans(string file) {

  int countTrans = 0;
  int reqs = 0;
  int resp = 0;
  
  try {

		//CSVReader<packetLen, trim_chars<' '>, no_quote_escape<'|'> > in( FILENAME.c_str() );
		CSVReader<packetLen> in( file.c_str() );
		while( in.read_row(
				   packet.frameNumber, packet.frameTimeRel, packet.frameTimeDelta,
				   packet.frameLen, packet.ipProto, packet.ipVersion, packet.ipSrc, packet.ethSrc,
				   packet.ipDst, packet.ethDst, packet.modbusUnitId, packet.tcpSrcPort,
				   packet.tcpDstPort, packet.mbtcpProtId, packet.mbtcpTransId, 
				   packet.mbtcpLen, packet.modbusFunctionCode, packet.modbusRefNum, 
				   packet.modbusWordCnt, packet.modbusData 
				   ) ) {

			// merge request and response packets
			if( packet.mbtcpTransId == prevPacket.mbtcpTransId ) {
        
				// request packet has destination port 502
				if( prevPacket.tcpDstPort == 502 ) {
          
					frNum[countTrans]=prevPacket.frameNumber;
					frameTimeRel[countTrans]=prevPacket.frameTimeRel;
					frameTimeDelta[countTrans]=prevPacket.frameTimeDelta;
					frameLen[countTrans]=prevPacket.frameLen;
					ipProto[countTrans]=prevPacket.ipProto;
					ipVersion[countTrans]=prevPacket.ipVersion;
					ipSrc[countTrans]=prevPacket.ipSrc;
					ethSrc[countTrans]=prevPacket.ethSrc;
					ipDst[countTrans]=prevPacket.ipDst;
					ethDst[countTrans]=prevPacket.ethDst;
					modbusUnitId[countTrans]=prevPacket.modbusUnitId;
					tcpSrcPort[countTrans]=prevPacket.tcpSrcPort;
					tcpDstPort[countTrans]=prevPacket.tcpDstPort;
					mbtcpProtId[countTrans]=prevPacket.mbtcpProtId;
					mbtcpTransId[countTrans]=prevPacket.mbtcpTransId;
					mbtcpLen[countTrans]=prevPacket.mbtcpLen;
					modbusFunctionCode[countTrans]=prevPacket.modbusFunctionCode;
					modbusRefNum[countTrans]=prevPacket.modbusRefNum;
					modbusWordCnt[countTrans]=prevPacket.modbusWordCnt;
//					frameSecond[countTrans]=(int)floor(prevPacket.frameTimeRel);
          
				} // if dstport=502
				
				// response packet has source port 502
				if( packet.tcpSrcPort == 502 ) {
          
					respFrameNumber[countTrans]=packet.frameNumber;
					respTimeRel[countTrans]=packet.frameTimeRel;
					respTimeDelta[countTrans]=packet.frameTimeDelta;
					respLen[countTrans]=packet.frameLen;
					respIpSrc[countTrans]=packet.ipSrc;
					respEthSrc[countTrans]=packet.ethSrc;
					respIpDst[countTrans]=packet.ipDst;
					respEthDst[countTrans]=packet.ethDst;
					respUnitId[countTrans]=packet.modbusUnitId;
					respSrcPort[countTrans]=packet.tcpSrcPort;
					respDstPort[countTrans]=packet.tcpDstPort;
					respProtId[countTrans]=packet.mbtcpProtId;
					respTransId[countTrans]=packet.mbtcpTransId;
          modbusData[countTrans]=packet.modbusData;

          countTrans++;
          
  			} // if srcport=502

			} // current and previous transid equal
			
			prevPacket = packet;
      
		} // end while read_row

  } catch (std::exception & e) {

		cout << "Exception mergeTrans() :\n" << endl;
		cout << e.what() << "\n" << endl;

	} // end try/catch

	return 0;

}

// [[Rcpp::export]]
DataFrame mergeProcess(DataFrame df, string file, string csv) {
  
  mergeTrans(file);
  
  DataFrame dfOut = DataFrame::create(
    
                Named("frNum")=frNum,
                Named("frameTimeRel")=frameTimeRel,
                Named("frameTimeDelta")=frameTimeDelta,
                Named("frameLen")=frameLen,
                Named("ipProto")=ipProto,
                Named("ipVersion")=ipVersion,
                /*
                Named("ipSrc")=ipSrc,
                Named("ethSrc")=ethSrc,
                Named("ipDst")=ipDst,
                Named("ethDst")=ethDst,
                Named("modbusUnitId")=modbusUnitId,
                Named("tcpSrcPort")=tcpSrcPort,
                Named("tcpDstPort")=tcpDstPort,
                Named("mbtcpProtId")=mbtcpProtId,
                Named("mbtcpTransId")=mbtcpTransId,
                Named("mbtcpLen")=mbtcpLen,
                Named("modbusFunctionCode")=modbusFunctionCode,
                Named("modbusRefNum")=modbusRefNum,
                Named("modbusWordCnt")=modbusWordCnt
*/
                      Named("respFrameNumber") = respFrameNumber,
                      Named("respTimeRel") = respTimeRel,
                      Named("respTimeDelta") = respTimeDelta,
                      Named("respLen") = respLen,
                      Named("respIpSrc") = respIpSrc,
                      Named("respEthSrc") = respEthSrc,
                      Named("respIpDst") = respIpDst, 
                      Named("respEthDst") = respEthDst,
                      Named("respUnitId") = respUnitId,
                      Named("respSrcPort") = respSrcPort,
                      Named("respDstPort") = respDstPort,
                      Named("respProtId") = respProtId,
                      Named("respTransId") = respTransId,
                      Named("modbusData") = modbusData
                      );
  
  return dfOut;

}

/*** R
library(microbenchmark)
library(data.table)


# call mergeProcess to merge transactions from pcap file and returns as DF

FILENAME <- "/home/lisa/scada/sew.data"
DATAFILE  <- "data.dat"
CSVFILE <- "dat.csv"

dat <- as.data.table(
  read.csv(FILENAME, header=TRUE,
           stringsAsFactors=T,
           colClass=c(ip.proto="factor", ip.version="factor", ip.src="factor",
                      ip.dst="factor", eth.src="factor", eth.dst="factor",
                      mbtcp.modbus.unit_id="factor",
                      tcp.srcport="factor", tcp.dstport="factor",
                      mbtcp.modbus.func_code="factor",
                      mbtcp.modbus.reference_num="factor",
                      mbtcp.prot_id="factor")))

# cleanup

dat <- dat[!(is.na(frame.number))]
dat <- dat[!(is.na(mbtcp.modbus.unit_id))]
dat <- dat[!(is.na(mbtcp.trans_id))]
dat <- dat[!(is.na(mbtcp.modbus.reference_num))]

write.table(dat, file=DATAFILE, row.names=FALSE, col.names = FALSE, sep = ",",
            quote = FALSE, na = "")

out <- mergeProcess(sewModbusDT, DATAFILE, CSVFILE)


rm(FILENAME, CSVFILE)

*/
