#include <stdio.h>
#include <stdlib.h>

int * getBackWay(int wayType) {
    int * step = calloc(2, sizeof(int));
    if (wayType == 2) {
        step[0] = 0;
        step[1] = 1;
        return step;
    }
    if (wayType == 3) {
        step[0] = 0;
        step[1] = -1;
        return step;
    }
    if (wayType == 4) {
        step[0] = 1;
        step[1] = 0;
        return step;
    }
    if (wayType == 5) {
        step[0] = -1;
        step[1] = 0;
        return step;
    }
}
int getWayType(int row_start,
                       int column_start,
                       int row_end,
                       int column_end) {

    int delta_row = row_end - row_start;
    int delta_column = column_end - column_start;
    if (delta_row == 0) {
        if (delta_column == 1)
            return 2;
        else return 3;
    } else {
        if (delta_row == 1)
            return 4;
        else return 5;
    };
}
int ** getNeighbors(int height, int width, int ** graph,
                        int row, int column) {
    int neighborsCount = 0;
    int ** neighbors = calloc(4, sizeof(long));
    if (row == 0) {
        neighbors[0] = 0;
    } else {
        char neighbor = graph[row - 1][column];
        if (neighbor == 0) {
            neighbors[0] = calloc(2, sizeof(int));
            neighbors[0][0] = row - 1;
            neighbors[0][1] = column;
            neighborsCount++;
        }


    }
    if (row == height - 1) {
        neighbors[1] = 0;
    } else {
        char neighbor = graph[row + 1][column];
        if (neighbor == 0) {
            neighbors[1] = calloc(2, sizeof(int));
            neighbors[1][0] = row + 1;
            neighbors[1][1] = column;
            neighborsCount++;
        }

    }
    if (column == 0) {
        neighbors[2] = 0;
    } else {
        char neighbor = graph[row][column - 1];
        if (neighbor == 0) {
            neighbors[2] = calloc(2, sizeof(int));
            neighbors[2][0] = row;
            neighbors[2][1] = column - 1;
            neighborsCount++;
        }
    }
    if (column == width - 1) {
        neighbors[3] = 0;
    } else {
        char neighbor = graph[row][column + 1];
        if (neighbor == 0) {
            neighbors[3] = calloc(2, sizeof(int));
            neighbors[3][0] = row;
            neighbors[3][1] = column + 1;
            neighborsCount++;
        }
    }
    return neighbors;
}

char * readLineFromFile(FILE * ОТКУДА_СЧИТЫВАТЬ) {
    char * getBufferPtr = calloc(256, sizeof(char));
    char * getBufferPtrMutable = getBufferPtr;
    char symbol;

    while(symbol = fgetc(ОТКУДА_СЧИТЫВАТЬ)) {
        if (symbol == '\n' || symbol == -1) {
            break;
        }

        *getBufferPtrMutable = symbol;
        getBufferPtrMutable++;
    }
    return getBufferPtr;
}

int main() {

    FILE * doc = fopen("maze1","r");

    int height;
    int width;
    char buffer[256];

    fscanf(doc,"%d", &height);
    fscanf(doc,"%d", &width);
    fgetc(doc); // КОНТРОЛЬНОЕ СЧИТЫВАНИЕ
    int ** graph = calloc(height, sizeof(long));
    for (int И = 0; И < height; ++И) {

        graph[И] = calloc(height, sizeof(char));
        char * stringPtr = readLineFromFile(doc);
        int counter = 0;
        while(*stringPtr) {
            if (*stringPtr == 0)
                break;
            if (*stringPtr == ' ') {
                stringPtr++;
                continue;
            }
            graph[И][counter] = *stringPtr-48;
            counter++;
            stringPtr++;
        }
    }
    char * startPos = readLineFromFile(doc);
    int startRow;
    int startColumn;
    sscanf(startPos, "%d %d", &startRow, &startColumn);
    startRow--;
    startColumn--;

    int ** Queue = calloc(1000, sizeof(long));
    int ** QueueStart = Queue;
    int ** QueueEnd = Queue;
    int QueueSize = 1;
    *Queue = calloc(2, sizeof(int));
    (*Queue)[0] = startRow;
    (*Queue)[1] = startColumn;
    while (QueueSize > 0) {
        int * SearchPosition = *QueueStart;
        QueueStart++;
        QueueSize--;
        int SearchRow = SearchPosition[0];
        int SearchColumn = SearchPosition[1];
        int ** neighBors = getNeighbors(height, width, graph,
                                                   SearchRow,
                                                   SearchColumn);

        for (int i = 0; i < 4; ++i) {
            if (neighBors[i] != 0) {
                QueueEnd++;
                (*QueueEnd) = neighBors[i];
                QueueSize++;
                int row_neighborА = neighBors[i][0];
                int column_neighborА = neighBors[i][1];
                int ПУТЬ = getWayType(SearchRow,
                                              SearchColumn,
                                              row_neighborА,
                                              column_neighborА);
                graph[row_neighborА][column_neighborА] = ПУТЬ;
            }
        }
    }
    char * endPosition = readLineFromFile(doc);
    int endRow;
    int endColumn;
    sscanf(endPosition, "%d %d", &endRow, &endColumn);
    endRow--;
    endColumn--;
    FILE * doc2 = fopen("maze1.res","w");
    if (graph[endRow][endColumn] == 0) {
        fprintf(doc2, "N");
        return 0;
    }
    int currentRow = endRow;
    int currentColumn = endColumn;
    fprintf(doc2, "Y\n");
    int ** answer = calloc(1000, sizeof(long));
    int ** answerBegin = answer;
    while(currentRow != startRow || currentColumn != startColumn) {
        int * step = getBackWay(graph[currentRow][currentColumn]);
        int stepRow = step[0];
        int stepColumn = step[1];
        (*answer) = calloc(2, sizeof(long));
        (*answer)[0] = currentRow+1;
        (*answer)[1] = currentColumn+1;
        currentRow -= stepRow;
        currentColumn -= stepColumn;

        answer++;
    }
    (*answer) = calloc(2, sizeof(long));
    (*answer)[0] = currentRow+1;
    (*answer)[1] = currentColumn+1;

    while (answer != answerBegin-1) {
        fprintf(doc2,"%d %d\n", (*answer)[0], (*answer)[1]);
        answer--;
    }
    return 0;
}
