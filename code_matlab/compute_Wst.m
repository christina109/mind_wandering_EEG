function [Wst, tlag, scale] = compute_Wst(data, times, plotOn)
% [Wst, tlag, scale] = compute_Wst(data, times, plotOn = 1)
% Convolving the input single-trial EEG data with the Mexican Hat function
% data: single trial EEG of lenght of nPnt
% times: time series (in ms) of the input data of length of nPnt
% plotOn: turn ON (defaul)/OFF plotting the convolutional result

% default
if nargin < 3 || isempty(plotOn) || ~ismember(plotOn, [0 1])
    plotOn = 1;
end

if ~isequal(size(data),size(times))
    error('Data and times must be the same size!')
end


% pars
srate = computeSrate(times);
tlag = 1:1000/srate:1000;
scale = 1:1000/srate:3500;

% intialize
Wst = zeros(length(scale),length(tlag));

% loop over t/s combo
for ti = 1:length(tlag)
    t = tlag(ti);
    for si = 1:length(scale)
        s = scale(si);
        wavelet = mexican_hat(times,t,s);
        %Wst(si, ti) = trapz(data.*wavelet).* (1000/srate) ./sqrt(s);
        Wst(si,ti) = sum(data .* wavelet)./sqrt(s);
    end
end

% plot
if plotOn == 1
    figure
    imagesc(tlag,scale,Wst)
    set(gca,'YDir','normal')
    %caxis([-50 50])
    ylabel('Scale (ms)')
    xlabel('Time lag (ms)')
    colorbar
end
        

end % func


function ps = mexican_hat(times, lag, scale)

t = (times-lag)./scale;
ps = (1- 16*t.^2).*exp(-8*t.^2);

end % funcs