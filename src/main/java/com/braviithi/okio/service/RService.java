package com.braviithi.okio.service;

import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.braviithi.okio.dto.ReportDto;

@Service
public class RService implements IRService {

	@Value("${okio.r.script}")
	private String rScript;
	
	@Value("${okio.s3.bucket}")
	private String bucketName;

	private String generateProcessCommand(String imageName, ReportDto reportDto) {
		
		if (StringUtils.hasText(imageName) && StringUtils.hasText(bucketName) && (reportDto != null)) {
			StringBuilder sb = new StringBuilder();
			sb.append("process('").append(imageName).append("', '").append(bucketName)
				.append("', ").append(reportDto.getVhMinValue()).append(", ").append(reportDto.getVhMaxValue())
				.append(", ").append(reportDto.getHeMinValue()).append(", ").append(reportDto.getHeMaxValue())
				.append(", ").append(reportDto.getHemValue())
				.append(", ").append(reportDto.getCwsMinValue()).append(", ").append(reportDto.getCwsMaxValue())
				.append(")");
			
			return sb.toString();
		}
		
		return null;
	}

	@Override
	public void processImage(String imageName, ReportDto reportDto) {
		RConnection connection = null;

		try {
			connection = new RConnection();

			connection.eval("source('" + rScript + "')");
			connection.eval(generateProcessCommand(imageName, reportDto));
		} catch (RserveException e) {
			e.printStackTrace();
		} finally {
			connection.close();
		}
	}

}
