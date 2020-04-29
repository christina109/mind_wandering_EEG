function computeSingleTrialERP(erp, subs, plotOn, condset, f_output)
% Compute W matrix and do local peak search to find the single trial ERP of interest. 
% Outputs are based on invididuals x conditions x tasks. 
% Note: the W in output file is integrated in the unit of sampling points to save computing time; the real W should be adjusted by W * 1000/srate.
% Author: Christina Jin (christina.mik109@gmail.com)

cd('c:\\topic_mind wandering\\3data')
load('pars.mat','times','srate')
load('pars_marker.mat', 'erps')

% set default
if nargin < 5 || isempty(f_output) 
    f_output = 'measure_matfile';
end
if nargin < 4 || isempty(condset)

    disp('Computing based on default conditions...')
    load('pars.mat', 'triggersets', 'tasks', 'states')
    defaultOn = 1;
    factors = {'state'};
    levels{1} = states;
else
     defaultOn = 0;
     triggersets = condset.triggersets;
     tasks       = condset.tasks;
     factors     = condset.factors;
     levels      = condset.levels; 
end


% get the erp definition
id = find(strcmp({erps.name}, erp));
measure = erps(id).name;
x2searchLower = erps(id).x2searchLower; 
x2searchUpper = erps(id).x2searchUpper; 
yRng2search   = erps(id).yRng2search;
sign  = erps(id).sign;
chans = erps(id).chans;
boundaryType  = erps(id).boundaryType; % 'float': [xLower RT]; 'fixed': [xLower xUpper]; 'adjusted': [xLower RT-->xUpper]

nlevel = zeros(1, length(factors));  % vector of level counts
for fi = 1:length(factors)
    nlevel(fi) = length(levels{fi});
end

% get level combos
if length(nlevel) > 1
    combos = zeros(prod(nlevel), length(nlevel));  % row = combo, column = factor, val = id in each factor
    i = length(nlevel);
    while i > 0
        if i == length(nlevel)
            combos(:,i) = repmat(1:nlevel(i), 1, size(combos,1)/nlevel(i));
        else
            temp = repmat(1:nlevel(i), prod(nlevel((i + 1):end)), 1);
            temp = temp(:)';
            combos(:,i) = repmat(temp, 1, size(combos,1)/length(temp));
        end
        i = i - 1;
    end 
else
    combos = [1:nlevel]';
end

n = length(chans) * size(combos,1) * length(tasks) * length(subs);
i = 0;
progressbar(['Computing single-trial ', upper(erp), '...'])

for chani = 1:length(chans)
    chan = chans(chani);
    for taski = 1:length(tasks)
    for comboi = 1:size(combos, 1)
        task = tasks{taski};
        
        if ~defaultOn
            slicebase = ['{', num2str(taski), '}']; 
            triggerset = {};
            cond = '';
            for fi = 1:size(combos, 2)
                eval(['triggerset{fi} = triggersets', slicebase, '{', num2str(fi), '}{', num2str(combos(comboi,fi)), '};'])
                if fi ~= size(combos,2)
                    cond = [cond, levels{fi}{combos(comboi, fi)}, '_'];
                else
                    cond = [cond, levels{fi}{combos(comboi, fi)}];
                end
            end
        else
            triggers = triggersets{taski}{comboi};
            cond = states{comboi};
        end
        
        for sub = subs
            
            % get data
            if defaultOn
              [data, idx, rts] = select_trials(sub, triggers);
            else              
               args = {};
               for fi = 1:length(factors)
                   args = [args, factors{fi}, triggerset{fi}];
               end
               [data, idx, rts] = select_trials2(sub, args);
            end
            
            if ~strcmp(boundaryType, 'fixed')
                 rts(rts==0)  = x2searchUpper; % adjust for SART: T
                 rts(rts>800) = x2searchUpper; % adjust for longer response time
            end

            mat = zeros(size(data,3),5); % colnames: s1Idx, s2Idx, val, t, s
            mat(:,1:2) = idx;

            for ni = 1:size(data,3) % loop over trial

                [id, session] = max(idx(ni,1:2));  
                trial = data(chan,:,ni);

                if strcmp(boundaryType, 'fixed')
                    xRng2search = [x2searchLower, x2searchUpper];
                else
                    xRng2search = [x2searchLower, rts(ni)];
                end

                if ni == 1
                    [W, xRng, yRng] = compute_Wst(trial,times,srate,0);
                else
                    W = compute_Wst(trial,times,srate,0);
                end
                [mat(ni,3), mat(ni,4), mat(ni,5)] = local_peak(W, xRng, yRng, xRng2search, yRng2search, sign, 0);

                if strcmp(boundaryType, 'adjusted')
                    if mat(ni,3) == 0
                       xRng2search = [x2searchLower, x2searchUpper];
                       [mat(ni,3), mat(ni,4), mat(ni,5)] = local_peak(W, xRng, yRng, xRng2search, yRng2search, sign, 0);
                    end
                end

                if plotOn == 1
                    figure
                    imagesc(xRng,yRng,W)
                    set(gca,'YDir','normal')
                    caxis([-50 50])
                    hold on 
                    x1 = xRng2search(1); y1 = yRng2search(1); x2 = xRng2search(end); y2 = yRng2search(end);
                    x = [x1, x2, x2, x1, x1];
                    y = [y1, y1, y2, y2, y1];
                    plot(x, y, 'k:', 'Color',[0.25 0.25 0.25], 'LineWidth', 2);
                    if isempty(mat(ni,3)) == 0
                        hold on
                        plot(mat(ni,4), mat(ni,5), '--ks','MarkerSize',5,'MarkerFaceColor','k');
                        hold on
                        text(mat(ni,4)+10, mat(ni,5), num2str(mat(ni,3)));
                    end
                    title(['Session ', num2str(session), ', trial ', num2str(id)])
                end
            end  % ni
            
            % output
            varName = [measure,'_',task,'_',cond];
            file = [f_output, '\\', measure, ' chan', num2str(chan), '\\', num2str(sub),'.mat'];
            eval([varName,'= mat;'])
            if exist(file,'file') == 2
                save(file, varName, '-append')
            else
                save(file, varName)
            end
            
            i = i + 1;
            progressbar(i/n) 
        end  % sub
    end  % comboi
    end  % taski
end  % chani

end
