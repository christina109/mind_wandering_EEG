function [data, idx, rt] = select_trials(sub, triggers, rmRejOn)
% extract trials in raw data based on the given triggers (eventnum)
% [data, idx, rt] = select_trials(sub, [triggers], rmRejOn = 1)
%  - data: nChan x nPnt x nTrial mat; 
%  - idx:  urevent  
%  - rt:   an array with length of nTrial
% last edit: Jun 5, 2018

% set defaults
if nargin == 2
    rmRejOn = 1;
end

% set path
cd('c:\\topic_mind wandering\\3data\\') % for the raw data
path2 = 'e:\\topic_mind wandering\\3data\\eeg_matfile\\'; % for the t-f decomposed data

for i = 1:2 %over session
    
    f = ['preprocessing\\' num2str(sub) '_' num2str(i) '_epochs_ica_a.set'];
    if exist(f, 'file') ~= 2
        f = ['preprocessing\\' num2str(sub) '_' num2str(i) '_epochs_ica_o_a.set'];
    end
    EEG = pop_loadset(f);

    if rmRejOn
        trials = ismember([EEG.event.new],triggers) & ~EEG.reject.rejmanual;
    else
        trials = ismember([EEG.event.new],triggers);
    end
        
    temp  = EEG.data(:,:,trials);
    temp2 = [EEG.event.urevent]; temp2 = temp2(trials);
    temp3 = [EEG.event.rt];      temp3 = temp3(trials);
    
    if i==1
        data = temp; 
        idx  = [temp2' zeros(length(temp2),1)];  % add a same length column with 0 
        rt   = temp3;
    else
        data(:,:,end+1:end+size(temp,3)) = temp;
        idx = [idx; [zeros(length(temp2),1) temp2']];
        rt  = [rt temp3];
    end
    
end %i: session

end %func