function [N,R] = simple_spatial_model(S,H,theta,A,w,z,k,error)
% STEG TA section: Simple Spatial Model
% This Matlab script solves a simple spatial model by inputing number of
% locations S, number of workers H, Frechet shape parameter theta,
% residential amenities A, wages/productivity w, housing elasticity z and
% k, and outputs spatial distribution N and residential rent R.
% Sally Zhang, Stanford University
% April 26, 2021

%% Parameters
%{
% The city consists of S locations and H workers. 
S = 3; H = 1;

% Frechet parameter
theta = 3;

% Residential amenities
A = [1 1 1];

% Marginal productivity
w = [1 1 1];

% Land supply elasticities
z = [1 1 1];
k = [1.1 1.2 1.3];

% Error tolerance
error = 0.0001;
%}

%% Loop
% initial guess of spatial distribution is that all workers are equally
% distributed across space
pi_0 = ones(1,S)./S;

% set up difference and iteration count
diff = 1;
iter = 1;

% loop until convergence
while diff>error
    
    % Compute spatial distribution of population
    N = pi_0.*H;
    
    % Calculate rent
    R = z + k.*N;
    
    % Calculate spatial distribution given rent
    pi_1 = (w.*A./R).^theta;
    pi_1 = pi_1./sum(pi_1);
    
    % Check convergence
    diff = max(abs(pi_0-pi_1));
    
    % Update rent
    pi_0 = 0.7.*pi_0 + 0.3.*pi_1;
    
    % counter
    iter = iter + 1;
end