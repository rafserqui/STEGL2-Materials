%% STEG TA section: Simple Spatial Model Simulation
% This Matlab script simulates results of a simple spatial model
% (simple_spatial_model.m) by varying exogenous parameters: Frechet shape
% parameter, residential amenities, producitivity and land supply
% elasticity.
% Sally Zhang, Stanford University
% April 26, 2021

clear all; close all; clc;

%% Parameters

% The city consists of S locations and H workers. 
S = 3; H = 1;

% Frechet parameter
theta = 3;
thetavar = linspace(1,5,10);

% Residential amenities
A = [1 1.5 2];
Avar = repmat(A,10,1);
Avar(:,1) = linspace(0.5,3,10);

% Marginal productivity
w = [1 1 1];
wvar = repmat(w,10,1);
wvar(:,1) = linspace(1,3,10);

% Land supply elasticities
z = [1 1 1];
k = [1 1 1];
kvar = repmat(k,10,1);
kvar(:,1) = linspace(0.5,2,10);

% Error tolerance
error = 0.0001;

%% Graph Frechet PDFs with different parameters
x = linspace(0.01,5,100);
pdf1 = -exp(-x.^(-1)).*(-1).*x.^(-2);
pdf2 = -exp(-x.^(-2)).*(-2).*x.^(-3);
pdf3 = -exp(-x.^(-5)).*(-5).*x.^(-6);
figure;
plot(x,pdf1,x,pdf2,x,pdf3);
legend('theta=1','theta=2','theta=5')

%% Simulations
% Amenities
N1 = zeros(10,S);
R1 = zeros(10,S);

for i = 1:10
    [N1(i,:),R1(i,:)] = simple_spatial_model(S,H,theta,Avar(i,:),w,z,k,error);
end

figure;
subplot(1,2,1);
plot(Avar(:,1),N1(:,1),Avar(:,1),N1(:,2),Avar(:,1),N1(:,3));
legend('Loc 1','Loc 2','Loc 3')
xlabel('Residential Amenities of Loc 1')
title('Population')
subplot(1,2,2);
plot(Avar(:,1),R1(:,1),Avar(:,1),R1(:,2),Avar(:,1),R1(:,3));
legend('Loc 1','Loc 2','Loc 3')
xlabel('Residential Amenities of Loc 1')
title('Rent')

% Productivity
N2 = zeros(10,S);
R2 = zeros(10,S);

for i = 1:10
    [N2(i,:),R2(i,:)] = simple_spatial_model(S,H,theta,A,wvar(i,:),z,k,error);
end

figure;
subplot(1,2,1);
plot(wvar(:,1),N2(:,1),wvar(:,1),N2(:,2),wvar(:,1),N2(:,3));
legend('Loc 1','Loc 2','Loc 3')
xlabel('Wages at Loc 1')
title('Population')
subplot(1,2,2);
plot(wvar(:,1),R2(:,1),wvar(:,1),R2(:,2),wvar(:,1),R2(:,3));
legend('Loc 1','Loc 2','Loc 3')
xlabel('Wages at Loc 1')
title('Rent')

% Housing elasticity
N3 = zeros(10,S);
R3 = zeros(10,S);

for i = 1:10
    [N3(i,:),R3(i,:)] = simple_spatial_model(S,H,theta,A,w,z,kvar(i,:),error);
end

figure;
subplot(1,2,1);
plot(kvar(:,1),N3(:,1),kvar(:,1),N3(:,2),kvar(:,1),N3(:,3));
legend('Loc 1','Loc 2','Loc 3')
xlabel('(Inverse) Housing elasticity at Loc 1')
title('Population')
subplot(1,2,2);
plot(kvar(:,1),R3(:,1),kvar(:,1),R3(:,2),kvar(:,1),R3(:,3));
legend('Loc 1','Loc 2','Loc 3')
xlabel('(Inverse) Housing elasticity at Loc 1')
title('Rent')

% Frechet parameter
N4 = zeros(10,S);
R4 = zeros(10,S);

for i = 1:10
    [N4(i,:),R4(i,:)] = simple_spatial_model(S,H,thetavar(i),A,w,z,k,error);
end

figure;
subplot(1,2,1);
plot(kvar(:,1),N4(:,1),kvar(:,1),N4(:,2),kvar(:,1),N4(:,3));
legend('Loc 1','Loc 2','Loc 3')
xlabel('Frechet distribution parameter')
title('Population')
subplot(1,2,2);
plot(kvar(:,1),R4(:,1),kvar(:,1),R4(:,2),kvar(:,1),R4(:,3));
legend('Loc 1','Loc 2','Loc 3')
xlabel('Frechet distribution parameter')
title('Rent')