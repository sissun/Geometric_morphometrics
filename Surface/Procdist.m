clear
%1. compute Prd between surfaces generated by sliding and nicp approaches.
SlidingMesh=triangulateMesh(loadMesh('./Data/Sliding-Target.obj')); %The surfaces derived using Sliding
center=mean(SlidingMesh.vertices);
SlidingMesh.vertices=SlidingMesh.vertices-center;
SlidingMesh.vertices=SlidingMesh.vertices./sqrt(sum(sum(SlidingMesh.vertices.^2, 2), 1)); 
   
NICPMesh=triangulateMesh(loadMesh('./Data/NICP.obj'));  %The surfaces derived using TPS&NICP
center=mean(NICPMesh.vertices);
NICPMesh.vertices=NICPMesh.vertices-center;
NICPMesh.vertices=NICPMesh.vertices./sqrt(sum(sum(NICPMesh.vertices.^2, 2), 1)); 
    
[error,allignedMesh,transform] = procrustes(SlidingMesh.vertices,NICPMesh.vertices); 
 %Procrutus 
A =(SlidingMesh.vertices-allignedMesh);
Prd = sqrt(sum(sum(A.^2, 2), 1));


%2. Register the surfaces derived using low density to surface by high density.
% When visualising the differnce between two surfaces��this step needs to be peformed.

SlidingMesh=triangulateMesh(loadMesh('./Data/Sliding-Target.obj')); %The surfaces derived using Sliding
SourceMesh=triangulateMesh(loadMesh('./Data/Sliding-Source.obj')); %The surfaces derived using low density
[error,allignedMesh,transform] = procrustes(SlidingMesh.vertices,SourceMesh.vertices,'scaling',true);% alignment two meshes
Savename=['./Data/Sliding-Source-register.obj'] %save as the aligned mesh 
fid=fopen(Savename,'w');
[m n]=size(Reallignedsource);
 for k=1:m
     fprintf(fid,'v %f %f %f\n',Reallignedsource(k,1),Reallignedsource(k,2),Reallignedsource(k,3));
 end
 [m,n]=size(SlidingMesh.faces);
 for k=1:m
     fprintf(fid,'f %d %d %d\n', SlidingMesh.faces(k,1), SlidingMesh.faces(k,2), SlidingMesh.faces(k,3));
 end
 fclose(fid);     
 
 