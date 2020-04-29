function power = compute_power(sig, baseidx, type)
% compute_power(sig, *baseidx=[], *type='db')
% compute relative power increase to the baseline
% sig: nChan x nPnt x nTrial complex matrix
% baseidx: in pnt
% type %in% {'0',[default]'db','%','z'}
% output is matrix with same size as sig

if nargin<3
    type='db';
    if nargin < 2
        baseidx = [];
    end
end

if isreal(sig)
    error('Do complex transform first!')
end

power = sig.*conj(sig);

if ~isempty(baseidx)
    
    base = repmat(mean(power(:,min(baseidx):max(baseidx),:),2),1,size(power,2),1);

    if strcmp(type,'db')
        disp('Db baseline correction')
        power = 10*log10(power./base);
    elseif strcmp(type,'%')
        disp('Percentage baseline correction')
        power = 100*(power-base)./base;  
    elseif strcmp(type,'z')
        error('Unfinished code!')
    end % type 
    
end
 
end % func
