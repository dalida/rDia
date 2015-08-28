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

// debug
//const bool check = 1;
const bool check = 0;

string FILENAME = "sew.dat";
string CSVFILE  = "sew.csv";

// number of variables in packet
const int packetLen = 20;

// packet
struct PacketStr {
	int    frameNumber;
	double frameTimeRel;
	double frameTimeDelta;
	int    frameLen;
	char   ipProto;
	char   ipVersion;
	string ipSrc;
	string ethSrc;
	string ipDst;
	string ethDst;
	string mbtcpModbusUnitId;
	string tcpSrcPort;
	string tcpDstPort;
	char   mbtcpProtId;
	int    mbtcpTransId;
	int    mbtcpLen;
	string mbtcpModbusFuncCode;
	string mbtcpModbusRefNum;
	int    mbtcpModbusWordCnt;
	string mbtcpModbusData;
} packet, prevPacket;

// merged mbtcp transaction
struct MbtcpTransStr {
  int    frameNumber;
  double frameTimeRel;
  double frameTimeDelta;
  int    frameLen;
  char   ipProto;
  char   ipVersion;
  string ipSrc;
  string ethSrc;
  string ipDst;
  string ethDst;
  string mbtcpModbusUnitId;
  string tcpSrcPort;
  string tcpDstPort;
	char   mbtcpProtId;
	int    mbtcpTransId;
	int    mbtcpLen;
	string mbtcpModbusFuncCode;
	string mbtcpModbusRefNum;
	int    mbtcpModbusWordCnt;
	int    frameSecond;
	int    respFrameNumber;
	double respTimeRel;
	double respTimeDelta;
	int    respLen;
	string respIpSrc;
	string respEthSrc;
	string respIpDest;
	string respEthDest;
	string respUnitId;
	string respSrcport;
	string respDstPort;
	char   respProtId;
	int    respTransId;
	int    respMbtcpLen;
	string respFuncCode;
	string mbtcpModbusData;
	int    respSecond;
	int    d;
} mbtcpTrans;

// all merged mbtcp transactions
vector<MbtcpTransStr> mergedTrans;

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

  NumericVector frNum();
  NumericVector frTimeRel();
  NumericVector frTimeDelta();
  IntegerVector frLen();
  CharacterVector ipProto();
  CharacterVector ipVersion();
  CharacterVector ipSrc();
  CharacterVector ethSrc();
  CharacterVector ipDst();
  CharacterVector ethDst();
  CharacterVector unitID();
  CharacterVector srcPort();
  CharacterVector dstPort();
  CharacterVector modbusProtId();
  IntegerVector transId();
  IntegerVector mbtcpLen();
  CharacterVector modbusFunction();
  CharacterVector modbusReference();
  IntegerVector modbusWordCnt();
  CharacterVector modbusData();

  int countTrans = 0;
	try {

		//CSVReader<packetLen, trim_chars<' '>, no_quote_escape<'|'> > in( FILENAME.c_str() );
		CSVReader<packetLen> in( file.c_str() );
		while( in.read_row(
				   packet.frameNumber, packet.frameTimeRel, packet.frameTimeDelta,
				   packet.frameLen, packet.ipProto, packet.ipVersion, packet.ipSrc, packet.ethSrc,
				   packet.ipDst, packet.ethDst, packet.mbtcpModbusUnitId, packet.tcpSrcPort,
				   packet.tcpDstPort,packet.mbtcpProtId, packet.mbtcpTransId, 
				   packet.mbtcpLen, packet.mbtcpModbusFuncCode,packet.mbtcpModbusRefNum, 
				   packet.mbtcpModbusWordCnt, packet.mbtcpModbusData 
				   ) ) {

			//cout<< "Current frame : " << packet.frameNumber<<endl;

			// merge request and response packets
			if( packet.mbtcpTransId == prevPacket.mbtcpTransId) {
				
				//cout<<"mbtcpTransId: "<< packet.mbtcpTransId<<"\n";
				// request packet has destination port 502
				if( prevPacket.tcpDstPort == "502" ) {
          
         frNum[countTrans] =  prevPacket.frameNumber;

/*
					mbtcpTrans.frameNumber  = prevPacket.frameNumber;
					mbtcpTrans.frameTimeRel = prevPacket.frameTimeRel;
					mbtcpTrans.frameTimeDelta= prevPacket.frameTimeDelta;
					mbtcpTrans.frameLen     = prevPacket.frameLen;
					mbtcpTrans.ipProto      = prevPacket.ipProto;
					mbtcpTrans.ipVersion    = prevPacket.ipVersion;
					mbtcpTrans.ipSrc        = prevPacket.ipSrc;
					mbtcpTrans.ethSrc       = prevPacket.ethSrc;
					mbtcpTrans.ipDst        = prevPacket.ipDst;
					mbtcpTrans.ethDst       = prevPacket.ethDst;
					mbtcpTrans.mbtcpModbusUnitId = prevPacket.mbtcpModbusUnitId;
					mbtcpTrans.tcpSrcPort   = prevPacket.tcpSrcPort;
					mbtcpTrans.tcpDstPort   = prevPacket.tcpDstPort;
					mbtcpTrans.mbtcpProtId  = prevPacket.mbtcpProtId;
					mbtcpTrans.mbtcpTransId = prevPacket.mbtcpTransId;
					mbtcpTrans.mbtcpLen     = prevPacket.mbtcpLen;
					mbtcpTrans.mbtcpModbusFuncCode = prevPacket.mbtcpModbusFuncCode;
					mbtcpTrans.mbtcpModbusRefNum   = prevPacket.mbtcpModbusRefNum;
					mbtcpTrans.mbtcpModbusWordCnt  = prevPacket.mbtcpModbusWordCnt;
					mbtcpTrans.frameSecond  = floor(prevPacket.frameTimeRel);
*/
				} // if dstport=502
				
				// response packet has source port 502
				if( packet.tcpSrcPort == "502" ) {
          
          cout << countTrans++ << endl;
          
					//cout<<"   Frame response: "<< packet.frameNumber<< "\n";
/*
					mbtcpTrans.respFrameNumber  = packet.frameNumber;
					mbtcpTrans.respTimeRel   = packet.frameTimeRel;
					mbtcpTrans.respTimeDelta = packet.frameTimeDelta;
					mbtcpTrans.respLen       = packet.frameLen;
					mbtcpTrans.respIpSrc     = packet.ipSrc;
					mbtcpTrans.respEthSrc    = packet.ethSrc;
					mbtcpTrans.respIpDest    = packet.ipDst;
					mbtcpTrans.respEthDest   = packet.ethDst;
					mbtcpTrans.respUnitId    = packet.mbtcpModbusUnitId;
					mbtcpTrans.respSrcport   = packet.tcpSrcPort;
					mbtcpTrans.respDstPort   = packet.tcpDstPort;
					mbtcpTrans.respProtId    = packet.mbtcpProtId;
					mbtcpTrans.respTransId   = packet.mbtcpTransId;
					mbtcpTrans.respMbtcpLen  = packet.mbtcpLen;
					mbtcpTrans.respFuncCode  = packet.mbtcpModbusFuncCode;
					mbtcpTrans.mbtcpModbusData = packet.mbtcpModbusData; 
					mbtcpTrans.respSecond    = floor(packet.frameTimeRel);
					mbtcpTrans.d = hexToDec(packet.mbtcpModbusData); 

					// add to all merged transactions
					mergedTrans.push_back(mbtcpTrans);
					
					// re-initialize transaction
					mbtcpTrans = (const struct MbtcpTransStr){ 0 };
*/


				} // if srcport=502
        

			} // current and previous transid equal
			
			prevPacket = packet;
			
		} // end while read_row

		// check
		if (check) {
			
			printf("number of merged trans: %d\n", mergedTrans.size());;
			cout<<"checking....\n";
			
			for( auto &i : mergedTrans ) {
				cout<< "------------";
				cout<< i.frameNumber << "," << endl;
				cout<< i.frameTimeRel << "," << endl;
				cout<< i.frameLen << "," << endl;
				cout<< i.ipProto << "," << endl;
				cout<< i.ipVersion << "," << endl;
				cout<< i.ipSrc << "," << endl;
				cout<< i.ethSrc << "," << endl;
				cout<< i.ipDst << "," << endl;
				cout<< i.ethDst << "," << endl;
				cout<< i.mbtcpModbusUnitId << "," << endl;
				cout<< i.tcpSrcPort << "," << endl;
				cout<< i.tcpDstPort << "," << endl;
				cout<< i.mbtcpProtId << "," << endl;
				cout<< i.mbtcpTransId << "," << endl;
				cout<< i.mbtcpLen << "," << endl;
				cout<< i.mbtcpModbusFuncCode << "," << endl;
				cout<< i.mbtcpModbusRefNum << "," << endl;
				cout<< i.mbtcpModbusWordCnt << "," << endl;
				cout<< i.frameSecond << "," << endl;
				cout<< i.respFrameNumber << "," << endl;
				cout<< i.respTimeRel << "," << endl;
				cout<< i.respTimeDelta << "," << endl;
				cout<< i.respLen << "," << endl;
				cout<< i.respIpSrc << "," << endl;
				cout<< i.respEthSrc << "," << endl;
				cout<< i.respIpDest << "," << endl;
				cout<< i.respEthDest << "," << endl;
				cout<< i.respUnitId << "," << endl;
				cout<< i.respSrcport << "," << endl;
				cout<< i.respDstPort << "," << endl;
				cout<< i.respProtId << "," << endl;
				cout<< i.respTransId << "," << endl;
				cout<< i.respMbtcpLen << "," << endl;
				cout<< i.respFuncCode << "," << endl;
				cout<< i.mbtcpModbusData << "," << endl;
				cout<< i.respSecond << "," << endl;
				cout<< i.d;

			} // end for mergedTrans
		} // end check

	} catch (std::exception & e) {

		cout << "Exception mergeTrans() :\n" << endl;
		cout << e.what() << "\n" << endl;

	} // end try/catch

	return 0;

} // end mergeTrans()

// create CSV file
// no need to create CSV if returning as DF
int createCSV(string csv) {

	try {

		ofstream csvfile;
		csvfile.open(csv);

		// write merged transactions to CSV file
		for( auto &i : mergedTrans ) {

			//cout<< i.frameNumber << endl;

			//csvfile << "," << i.frameNumber; // comma before for mysqldb
			csvfile << i.frameNumber;
			csvfile << "," << i.frameTimeRel;
			csvfile << "," << i.frameTimeDelta;
			csvfile << "," << i.frameLen;
			csvfile << "," << i.ipProto;
			csvfile << "," << i.ipVersion;
			csvfile << "," << i.ipSrc;
			csvfile << "," << i.ethSrc;
			csvfile << "," << i.ipDst;
			csvfile << "," << i.ethDst;
			csvfile << "," << i.mbtcpModbusUnitId;
			csvfile << "," << i.tcpSrcPort;
			csvfile << "," << i.tcpDstPort;
			csvfile << "," << i.mbtcpProtId;
			csvfile << "," << i.mbtcpTransId;
			csvfile << "," << i.mbtcpLen;
			csvfile << "," << i.mbtcpModbusFuncCode;
			csvfile << "," << i.mbtcpModbusRefNum;
			csvfile << "," << i.mbtcpModbusWordCnt;
			csvfile << "," << i.frameSecond;
			csvfile << "," << i.respFrameNumber;
			csvfile << "," << i.respTimeRel;
			csvfile << "," << i.respTimeDelta;
			csvfile << "," << i.respLen;
			csvfile << "," << i.respIpSrc;
			csvfile << "," << i.respEthSrc;
			csvfile << "," << i.respIpDest;
			csvfile << "," << i.respEthDest;
			csvfile << "," << i.respUnitId;
			csvfile << "," << i.respSrcport;
			csvfile << "," << i.respDstPort;
			csvfile << "," << i.respProtId;
			csvfile << "," << i.respTransId;
			csvfile << "," << i.respMbtcpLen;
			csvfile << "," << i.respFuncCode;
			csvfile << "," << i.mbtcpModbusData;
			csvfile << "," << i.respSecond;
			csvfile << "," << i.d;
			csvfile << "\n";
			
		} // end for mergedTrans
		
		csvfile.close();
		
	} catch(std::exception& e) {

		cout << "Exception createCSV() : \n";
		cout << "Exception mergeTrans() :\n";
		cout << e.what() << endl;
		
	} // end try/catch

	return 0;

} // createCSV()

// [[Rcpp::export]]
DataFrame mergeProcess(DataFrame df, string file, string csv) {
	// add parameters for pcap and csv filenames
	// check input parameters

	// merge transactions
  /*
  IntegerVector frNum = df["frame.number"];
  NumericVector frTimeRel = df["frame.time_relative"];
  NumericVector frTimeDelta = df["frame.time_delta"];
  IntegerVector frLen = df["frame.len"];
  CharacterVector ipProto = df["ip.proto"];
  CharacterVector ipVersion = df["ip.version"];
  CharacterVector ipSrc = df["ip.src"];
  CharacterVector ethSrc = df["eth.src"];
  CharacterVector ipDst= df["ip.dst"];
  CharacterVector ethDst= df["eth.dst"];
  CharacterVector unitID = df["mbtcp.modbus.unit_id"];
  CharacterVector srcPort = df["tcp.srcport"];
  CharacterVector dstPort = df["tcp.dstport"];
  CharacterVector modbusProtId = df["mbtcp.prot_id"];
  IntegerVector transId = df["mbtcp.trans_id"];
  IntegerVector mbtcpLen = df["mbtcp.len"];
  CharacterVector modbusFunction = df["mbtcp.modbus.func_code"];
  CharacterVector modbusReference = df["mbtcp.modbus.reference_num"];
  IntegerVector modbusWordCnt = df["mbtcp.modbus.word_cnt"];
  CharacterVector modbusData = df["mbtcp.modbus.data"];
  */

  
  mergeTrans(file);

  DataFrame dfOut = DataFrame::create(Named("frame.number")=frNum
  );

//  DataFrame dfOut = DataFrame::create(Named("frame.number")=frNum,
//                                      Named("frame.time_relative")=frTimeRel,
//                                      Named("frame.time_delta")=frTimeDelta,
//                                      Named("frame.len")=frLen,
//                                      Named("ip.proto")=ipProto,
//                                      Named("ip.version")=ipVersion
//                                      );

  
//  return dfOut;
  return dfOut;
}

/*** R
library(microbenchmark)
library(data.table)


# call mergeProcess to merge transactions from pcap file and returns as DF

#D <- data.frame(a=1:3, b=LETTERS[1:3], c=Sys.Date()+0:2, stringsAsFactors=FALSE)
#print(D)

FILENAME <- "/home/lisa/scada/sew.data.1"
CSVFILE  <- "sew.csv"
#summary(sewModbusDT)
out <- mergeProcess(sewModbusDT, FILENAME, CSVFILE)

*/
