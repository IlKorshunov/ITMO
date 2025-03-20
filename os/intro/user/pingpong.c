#include "user.h"
#define BUFF 1024

void send(int* fd, const char* input, int len) {
  close(fd[0]);
  int ex = write(fd[1], input, len);
  close(fd[1]);
  if (ex != len) {
    printf("error during recording");
    exit(-1);
  }
}

void get(int* fd) {
  close(fd[1]);
  char buffer[BUFF];
  int ex = read(fd[0], buffer, BUFF - 1);
  printf("%d: got ", getpid());
  while (ex > 0) {
    buffer[ex] = '\0';
    printf(buffer);
    ex = read(fd[0], buffer, BUFF - 1);
  }
  printf("\n");
  close(fd[0]);
}

int main() {
  char str1[] = "ping";
  char str2[] = "pong";

  int fd1[2];
  int fd2[2];
  if (pipe(fd1) == -1) {
    printf("error in first pipe");
    exit(-3);
  }

  if (pipe(fd2) == -1) {
    printf("error in second pipe");
    exit(-4);
  }

  int pid = fork();

  if (pid > 0) {
    send(fd1, str1, strlen(str1));
    get(fd2);
  } else if (pid == 0) {
    get(fd1);
    send(fd2, str2, strlen(str2));
  } else {
    printf("error in fork");
    exit(-5);
  }
  exit(0);
}