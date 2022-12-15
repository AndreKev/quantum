
#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

// gcc msocket.cpp -lwsock32 -lWs2_32 -o server && ./server localhost
#undef UNICODE

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <stdlib.h>
#include <stdio.h>

//#include <Rcpp.h>

// Prototypes
char* _cwait(const char*);

// Need to link with Ws2_32.lib
#pragma comment (lib, "Ws2_32.lib")
// #pragma comment (lib, "Mswsock.lib")

#define DEFAULT_BUFLEN 512
//#define DEFAULT_PORT "27015"

// [[Rcpp::export]]
CharacterVector cwait(CharacterVector R_PORT) {
  String PORT = as<String>(R_PORT);
  char *out = _cwait(PORT.get_cstring());
  return wrap((String)out) ;
}


// Sub process called in cwait
char* _cwait(const char* DEFAULT_PORT)
{
  static char ERROR_MESSAGE[DEFAULT_BUFLEN];
  WSADATA wsaData;
  int iResult;

  SOCKET ListenSocket = INVALID_SOCKET;
  SOCKET ClientSocket = INVALID_SOCKET;

  struct addrinfo *result = NULL;
  struct addrinfo hints;

  int iSendResult;
  static char recvbuf[DEFAULT_BUFLEN];
  int recvbuflen = DEFAULT_BUFLEN;

  // Initialize Winsock
  iResult = WSAStartup(MAKEWORD(2,2), &wsaData);
  if (iResult != 0) {
    printf("WSAStartup failed with error: %d\n", iResult);
    return ERROR_MESSAGE;
  }

  ZeroMemory(&hints, sizeof(hints));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = IPPROTO_TCP;
  hints.ai_flags = AI_PASSIVE;

  // Resolve the server address and port
  iResult = getaddrinfo(NULL, DEFAULT_PORT, &hints, &result);
  if ( iResult != 0 ) {
    printf("getaddrinfo failed with error: %d\n", iResult);
    WSACleanup();
    return ERROR_MESSAGE;
  }

  // Create a SOCKET for the server to listen for client connections.
  ListenSocket = socket(result->ai_family, result->ai_socktype, result->ai_protocol);
  if (ListenSocket == INVALID_SOCKET) {
    printf("socket failed with error: %d\n", WSAGetLastError());
    freeaddrinfo(result);
    WSACleanup();
    return ERROR_MESSAGE;
  }

  // Setup the TCP listening socket
  iResult = bind( ListenSocket, result->ai_addr, (int)result->ai_addrlen);
  if (iResult == SOCKET_ERROR) {
    printf("bind failed with error: %d\n", WSAGetLastError());
    freeaddrinfo(result);
    closesocket(ListenSocket);
    WSACleanup();
    return ERROR_MESSAGE;
  }

  freeaddrinfo(result);

  iResult = listen(ListenSocket, SOMAXCONN);
  if (iResult == SOCKET_ERROR) {
    printf("listen failed with error: %d\n", WSAGetLastError());
    closesocket(ListenSocket);
    WSACleanup();
    return ERROR_MESSAGE;
  }

  // Accept a client socket
  ClientSocket = accept(ListenSocket, NULL, NULL);
  if (ClientSocket == INVALID_SOCKET) {
    printf("accept failed with error: %d\n", WSAGetLastError());
    closesocket(ListenSocket);
    WSACleanup();
    return ERROR_MESSAGE;
  }

  // No longer need server socket
  closesocket(ListenSocket);

  // Receive until the peer shuts down the connection
  do {

    iResult = recv(ClientSocket, recvbuf, recvbuflen, 0);
    if (iResult > 0) {
      //printf("Bytes received: %d\n", iResult);
      //printf("%s\n", recvbuf);
      // Echo the buffer back to the sender
      iSendResult = send( ClientSocket, recvbuf, iResult, 0 );
      if (iSendResult == SOCKET_ERROR) {
        printf("send failed with error: %d\n", WSAGetLastError());
        closesocket(ClientSocket);
        WSACleanup();
        return ERROR_MESSAGE;
      }
      //printf("Bytes sent: %d\n", iSendResult);
    }
    else if (iResult == 0)
      //pass
      continue;
    //printf("Connection closing...\n");
    else  {
      printf("recv failed with error: %d\n", WSAGetLastError());
      closesocket(ClientSocket);
      WSACleanup();
      return ERROR_MESSAGE;
    }

  } while ((iResult > 0));

  // shutdown the connection since we're done
  iResult = shutdown(ClientSocket, SD_SEND);
  if (iResult == SOCKET_ERROR) {
    printf("shutdown failed with error: %d\n", WSAGetLastError());
    closesocket(ClientSocket);
    WSACleanup();
    return ERROR_MESSAGE;
  }

  // cleanup
  closesocket(ClientSocket);
  WSACleanup();
  return recvbuf;
}
