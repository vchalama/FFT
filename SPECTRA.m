clc;
clear all;
close all;
omega = 0.15

load -ASCII U_FOURIER.dat 


plot(U_FOURIER(:,1)/omega, U_FOURIER(:,2));

% % Create legend
%legend ('Fr = 0.061', 'Fr = 0.148','Fr = 0.3');

% % Create ylabel
xlabel('\omega / \Omega','FontSize',40, 'FontName','Times');

% % Create ylabel
ylabel('P(\omega/\Omega)','FontSize',40, 'FontName','Times');
 
set(gca,'FontSize',40, 'LineWidth',1,'FontName','Times');


axis([0 5 0 0.3])
