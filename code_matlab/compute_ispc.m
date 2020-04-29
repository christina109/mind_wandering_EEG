function [ispc, lag] = compute_ispc(data, freq)
% [ispc, lag] = compute_ispc(data, *freq)
% compute inter-site phase clustering across time between rows (chans)
% Note. If 2 chans has high ISPC & phase-lag nearly == 0, the coherence
%       might be caused by volumn conductance. To exclude volumn
%       conduction, use phase-lag coherence instead. Moreover, for the 
%       directional phase-lag coherence, check the data2psiX.m
% data: nChan x nPnt x nTrial complex
% ispc / lag: nChan x nChan x nTrial
% phase-lag can be further used to do gv-test

phase = angle(data);
nChan = size(data, 1);

% initialze
ispc = zeros(nChan, nChan, size(data,3));
lag  = ispc;

for chani = 1 : nChan-1
    for chanj = chani + 1 :nChan
        phase_sig1   = phase(chani,:,:); 
        phase_sig2   = phase(chanj,:,:);
        phase_diffs  = phase_sig1 - phase_sig2;
        meanDiffVec  = mean(exp(1i * phase_diffs), 2);  % averaging the diff vector space across pnts
        ispc(chani, chanj, :) = abs(meanDiffVec);  
        lag (chani, chanj, :) = angle(meanDiffVec);
        ispc(chanj, chani, :) = ispc(chani, chanj, :);  % flip along diagnol
        lag (chanj, chani, :) = lag(chani, chanj, :);
    end
end

if ~isempty(freq)
    lag = 1000 * lag / (2*pi*freq);
end

end  % func