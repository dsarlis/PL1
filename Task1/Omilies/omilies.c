#include<stdio.h>
#include<stdlib.h>

#define MaxInt 1000000000
#define MAX 10000


typedef struct cellStruct{
	int dist;
	int city;			
	struct cellStruct *next;
} cellType;

typedef cellType *CellPointer;


int min (int dist[MAX],int visited[MAX],int N){
	int min=MaxInt,minPos=0,i=0;

	//this function finds the min of an array and returns its position
	for (i=0; i<N;i++){
		if ((dist[i] < min )&&(visited[i]!=1)){
			minPos = i;
			min = dist[i];
		}
	}
	return minPos;
}



void dijkstra(CellPointer *Graph,int source, int N,int dist[MAX]){
	int u=-1,i,visited[MAX],alt=MaxInt,*udist = NULL,*pdist = NULL,*p=NULL;
	CellPointer ptr=NULL;

	//this function computes the shortest paths for each vertex using Dijkstra's algorithm

	for (p=dist;p<dist+N;p++)
		*p = MaxInt;
	for (i=0;i<N;i++)		//initialise the distance of each vertex to infinity
		visited[i] = 0; 	
	 *(dist+source) = 0;		//and the distance of the target vertex (from itself) is set to 0

	i = N;
	
	while (N>0){				//we repeat te following until there is no other vertex in the graph
		u = min(dist,visited,i);	//find the vertex with min distance for target vertex
		udist = dist+u;
		if (*udist == MaxInt) 
			break;
		N--;
		ptr = *(Graph + u);
		while (ptr!=NULL){		//for each neighbour of the min vertex we compute if there is a better path from it to the target vertex
			pdist = dist + ptr->city;
			alt = ptr->dist + dist[u];
			if (alt < *pdist)	//if there is we update its distance from the target vertex
				*pdist = alt;
			ptr = ptr->next;
		}	
		visited[u] = 1; 	//mark this vertex as visited so we don't visit it again
	}
	
}


void insertElement (int city1,int city2,int cost,CellPointer *Graph){
	cellType *Node = NULL;

	//this function inserts a new element in our graph
	//we update both cities connected because each route goes both-ways

	Node = (cellType *)malloc(sizeof(cellType));

        Node->dist = cost;
        Node->city = city2;
        Node->next = NULL;

        if (*(Graph+city1) == NULL)
             *(Graph+city1) = Node;
        else {
             Node->next = *(Graph+city1);
             *(Graph+city1) = Node;
        }

}


int main (int argc, char* argv[])
{

	/*We use a dynamic array of lists to represent the graph
	 Each element of the array is a list which contains the neighbours of each vertex
	 and the costs of the paths from one vertex to the other 
	*/
	int cities=0, connections=0, questNo=0, i=0;
	int cityA=0, cityB=0, cityC=0,x=0,y=0;
	int  z=0,Adist[MAX], Bdist[MAX], Cdist[MAX];
	CellPointer  *array=NULL;
	int testA=-1,testB=-1,testC=-1,flag=0,j=0;
	FILE *fpin;	

	
	fpin = fopen(argv[1], "r");
        if (fscanf(fpin, "%d %d", &cities, &connections)==EOF)      //we read the number of vertices and edges
		printf("Wrong file format1\n");

        array = (CellPointer *)malloc(cities*sizeof(CellPointer));
        for(i=0; i<cities; i++){
                *(array+i) = NULL;
        }

        for(i=0; i<connections; i++){
              if  (fscanf(fpin, "%d %d %d", &x, &y, &z)==EOF)     //we read the connections among the nodes and the cost of each connection
			printf("Wrong file format2\n");
	      insertElement(x-1,y-1,z,array);
	      insertElement(y-1,x-1,z,array);
        }

	

        if (fscanf(fpin, "%d %d %d %d", &cityA,&cityB,&cityC,&questNo)==EOF)
		printf("Wrong file format3\n");
	cityA--;
	cityB--;									//we read the three cities A,B,C and the number of questions
        cityC--;

	dijkstra(array,cityA,cities,Adist);      //we use Dijkstra's algorithm for finding the shortest paths from each city A,B,C to every other city
        dijkstra(array,cityB,cities,Bdist);
        dijkstra(array,cityC,cities,Cdist);	 	


        for(i=0; i<questNo; i++){
              if  (fscanf(fpin, "%d", &x)==EOF)
			printf("Wrong file format4\n");
	      testA = Adist[x-1];
	      testB = Bdist[x-1];	
	      testC = Cdist[x-1];
	      flag = 0;
	      j = 0;								//for each question we check if there is a city from which the three cities	
	      while ((flag==0)&&(j<cities)){				        //A,B,C are all closer than the city we are asked for
	            if ((Adist[j]<testA)&&(Bdist[j]<testB)&&(Cdist[j]<testC)){  //if such a city exists then we must answer false because the city we are asked for is not   
			flag=1;							//possible for a speech else we must answer true
		    }
		    j++;
	      }
	      if (flag==1)
	           printf("false\n");
	      else printf("true\n");  
        }


        fclose(fpin);

	
	return 0;
}

