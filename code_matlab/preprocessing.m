function EEG = preprocessing(data, sessionnum, stage)
% preprocessing(data, sessionnum, stage)
% stage 1: importing data, filtering, downsampling, interpolation, epoching
% stage 2: (optional) removing artifacts mannaully before ICA
% stage 3: running ica
% stage 4: remove artifact ICA cmps
% stage 5: check for artifacts again
% stage 6: (optional) edit triggers

cd('c:\\topic_mind wandering\\3data')

% check input pars
if numel(stage)~=1
    error('Each time process only one stage!')
end

% set pars
chanlocs_file = 'biosemi128.ced';
path2         = 'e:\\topic_mind wandering\\3data\\'; % for raw data
pars_file     = 'pars_preprocessing.mat'; % for saving/loading some individual pars
srate         = 256; 
bandpass      = [0.5 40]; % after 11/18/2017, bandpass = [0.1 42]
epoch         = [-0.4 1.2]; %in seconds
triggers      = {'10','11','12','13','14','20','21'}; %raw triggers; they will be re-reedited later
cmps2plot     = 25:-1:1;

for i = data
    for j = sessionnum
        
        if stage == 1

            % import raw
            EEG = pop_biosig([path2,'raw\\eeg\\sub',num2str(i),'_',num2str(j),'.bdf'],'ref',[133 134],'refoptions',{'keepref' 'off'}); %import & reref

            % edit channel struc
            EEG = pop_chanedit(EEG, 'lookup',chanlocs_file); %load channal info
            EEG = pop_select(EEG, 'nochannel', 129:134); %remove extra channels because there is no coordinates for these channels which might affects ICA
            EEG = pop_chanedit(EEG, 'setref', {'1:128' 'TP9 TP10'}); %set reference info: mastoids

            % save 
            EEG = pop_saveset( EEG,['preprocessing\\',num2str(i),'_',num2str(j),'_reref.set']);

            % interpolate bad channels 
            load(pars_file,'chancorrect')
            rowno = find([chancorrect{:,1}]==i & [chancorrect{:,2}]==j);
            if isempty(rowno)
                disp('No bad channels...');
                bad_chan  = [];
                srnd_chan = [];
            else
                bad_chan  = chancorrect{rowno,3};
                srnd_chan = chancorrect{rowno,4};
            end
            for k = 1:length(bad_chan)
              bad  = bad_chan(k);
              srnd = srnd_chan(k,:);
              srnd(isnan(srnd)) = [];
              EEG  = bad_chan_correct(EEG, bad, srnd); % to correct the bad channel by interpolation
            end

            % band-pass filtering
            EEG = pop_eegfiltnew(EEG, min(bandpass), max(bandpass));

            % down-sampling
            EEG = pop_resample(EEG, srate);

            % extract epochs
            EEG = pop_epoch(EEG, triggers, epoch); 
            EEG = pop_rmbase(EEG, [epoch(1)*1000 0]);
            if EEG.trials ~= 825
                error('Incorrect trigger number!')
            end

            % save 
            pop_saveset(EEG,['preprocessing\\',num2str(i),'_',num2str(j),'_epochs.set']);
        
        end
        
        %%
        
        if stage == 2
        
            % load data
            EEG = pop_loadset(['preprocessing\\',num2str(i),'_',num2str(j),'_epochs.set']);
            load(pars_file, 'trej_beforeICA')
            % this stage including the var trej_beforeICA is newly designed
            % before the last time I ran the preprocessing code
            
            % maybe remove artifacts before ica?
            disp ('pop_eegplot(EEG);')
            disp (['trej_beforeICA{', num2str(i), ',', num2str(j), '} = find(EEG.reject.rejmanual);'])
            disp (['save(''', pars_file, ''', ''trej_beforeICA'', ''-append'');'])
            disp (['pop_saveset(EEG, [''preprocessing\\',num2str(i),'_',num2str(j),'_epochs.set''])'])  % to save the markers before deleting the marked epochs
            disp ('EEG = pop_rejepoch(EEG, EEG.reject.rejmanual);')
            disp (['pop_saveset(EEG, [''preprocessing\\',num2str(i),'_',num2str(j),'_epochs_a.set''])'])

        end
        
        %%
        
        if stage == 3
            
            % load 
            f = ['preprocessing\\',num2str(i),'_',num2str(j),'_epochs_a.set'];
            if exist(f, 'file') == 2
              EEG = pop_loadset(f); 
            else 
              EEG = pop_loadset(['preprocessing\\',num2str(i),'_',num2str(j),'_epochs.set']);
            end

            % run ica 
            EEG = pop_runica(EEG, 'extended', 1, 'stop', 1E-7);

            % save 
            pop_saveset(EEG, ['preprocessing\\' num2str(i),'_',num2str(j),'_epochs_ica.set']);
            % after this step, the [sub]_[session]_epoches[_a].set can be
            % deleted to save the storage, because this output data only
            % adds information to the EEG structure in the previous data 

        end
        
        %%
        
        if stage == 4
            
            % load 
            EEG = pop_loadset(['preprocessing\\',num2str(i),'_',num2str(j),'_epochs_ica.set']);

            % plot cmps
           	disp(['pop_prop(EEG, 0,[', num2str(cmps2plot),'], NaN,{''freqrange'' [0.5 40]});'])
            
            % record the artifact comps
            disp(['load(''', pars_file, ''',''rm_comp'')'])
            disp(['rm_comp{',num2str(i),',',num2str(j),'} = find(EEG.reject.gcompreject);'])
            disp(['save(''', pars_file, ''',''rm_comp'',''-append'')'])
            
            % remove cmps            
            disp('EEG = pop_subcomp(EEG);')

            % save 
            disp(['pop_saveset(EEG, [''preprocessing\\',num2str(i),'_',num2str(j),'_epochs_ica_a.set'']);'])
            % here is a new change; previously, the data were saved as
            % [sub]_[session]_epochs_ica_o.set
            
        end
        
        %%
        
        if stage == 5
            
            % load 
            EEG = pop_loadset(['preprocessing\\',num2str(i),'_',num2str(j),'_epochs_ica_a.set']);
            
            % mark the remaining artifacts
            disp ('pop_eegplot(EEG);')
            disp ('EEG = pop_rejepoch(EEG,EEG.reject.rejmanual);')
            
            % save 
            disp(['pop_saveset(EEG,[''preprocessing\\',num2str(i),'_',num2str(j),'_epochs_ica_a.set'']);'])
            % this is the almost the final output, the next step only adds
            % more info to the data structure
            % previoulsy, the output here is named as
            % [sub]_[session]_epochs_ica_o_a.set
            
        end
        
        %%
        
        if stage == 6
            
            edit_triggers(i,j)
        
        end
    
    end % j: session 
end % i: sub

end %func

